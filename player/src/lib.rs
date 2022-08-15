/**
 * sometypes bomber bot
 *
 * One day of hacking, starting at 13:30 on the day of the event.
 * Plus a postmortem commit to clean up egregious copypasting and add commentary.
 *
 * General approach:
 *  - Deliberately avoid A*, numerical heuristics, and new non-std libraries. For fun and coding speed.
 *  - Cache observed info on world. Required due to fog of war.
 *  - Stateless behavior. No intention held across ticks.
 *  - Pathfinding: Multi-pass BFS with changing objectives and criteria for tile traversability.
 *  - Accidental pacifist: No attempt to actively attack others or go after powerups, due to time constraints.
 *  - DRY ignored for maximum hacking. BFS code copy-pasted many times, then cleaned up in this postmortem.
 *
 * Outstanding bugs:
 *  - No consideration of fuse time. Wanted to bake these into a list-per-coord of future explosion times.
 *  - Objects that have left visibility aren't considered for movement/safety. Causes occasional suicide.
 *  - Using stateless BFS sometimes causes routes to oscillate between two different directions and get stuck.
 *      With more time available, either switch to A* or retain an intended path across ticks.
 */
use bomber_lib::{
  self,
  world::{Direction, Enemy, Object, Tile, TileOffset},
  Action, Player,
};
use bomber_macro::wasm_export;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};

// An absolute position, with origin at spawn location.
#[derive(
  Copy, Clone, Debug, Default, Eq, PartialEq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Coord(pub i32, pub i32);

// Check whether this location is a place we want to go.
type GoalPredicate = fn(&MyPlayer, &Coord) -> bool;

// Check whether we're allowed to move here.
type StepAllowedPredicate = fn(&MyPlayer, &Coord) -> bool;

/// Player struct. Can contain any arbitrary data, which will carry over between turns.
#[derive(Default)]
struct MyPlayer {
  // Our current absolute position
  curr_abs_pos: Coord,
  // Terrain info for the tiles we've seen before
  seen_terrain: HashMap<Coord, Tile>,
  // If we've found a hill, where is it?
  known_hill: Option<Coord>,
  // Where are the unexplored map areas we can get to?
  frontier: HashSet<Coord>,

  // Crates we've seen
  visible_crates: HashSet<Coord>,
  // Enemies we've seen
  visible_enemies: HashSet<Coord>,
  // Powerups we've seen
  visible_powerups: HashSet<Coord>,
  // Tiles that are getting exploded
  future_explosions: HashMap<Coord, u32>,
}

#[wasm_export]
impl Player for MyPlayer {
  fn act(
    &mut self,
    surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, TileOffset)>,
  ) -> Action {
    // Observe the world.
    self.remember_terrain(surroundings.clone());
    self.remember_objects(surroundings);

    // If we've seen a hill, try to move there while avoiding crates.
    if self.known_hill != None && let Some(next) = self.next_step_to_hill(self.curr_abs_pos) {
      return self.consider_moving(next);
    }

    // If we've seen a hill, try to move there through crates.
    if self.known_hill != None && let Some(next) = self.next_step_to_hill_through_crates(self.curr_abs_pos) {
      return self.consider_moving(next);
    }

    // If no hill visible, look for frontier (unexplored world areas) to move to.
    if let Some(next) = self.next_step_to_frontier(self.curr_abs_pos) {
      return self.consider_moving(next);
    }

    // No reachable frontier. Idle. Shouldn't get here on sane maps.
    return self.consider_moving(self.curr_abs_pos);
  }

  fn name(&self) -> String {
    String::from("sometypes")
  }

  fn team_name() -> String {
    String::from("bocchi")
  }
}

impl MyPlayer {
  // Get an absolute coordinate for the given player-relative coordinate.
  fn get_coord(&self, offset: TileOffset) -> Coord {
    Coord(offset.0 + self.curr_abs_pos.0, offset.1 + self.curr_abs_pos.1)
  }

  // Remember the objects we can see now.
  fn remember_objects(
    &mut self,
    surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, TileOffset)>,
  ) {
    // Normally these should be retained until they're observed to have changed.
    // No time to debug cache details so we're wiping them every tick and just using what we can see now.
    self.future_explosions.clear();
    self.visible_powerups.clear();
    self.visible_crates.clear();
    self.visible_enemies.clear();

    for contents in surroundings {
      let coord = self.get_coord(contents.3);
      match contents.1 {
        Some(Object::Bomb { fuse_remaining: _, range }) => {
          // Dumb: ignoring fuse time for now
          for r in 0..=range {
            let r = r as i32;
            self.future_explosions.insert(Coord(coord.0 + r, coord.1), 1);
            self.future_explosions.insert(Coord(coord.0 - r, coord.1), 1);
            self.future_explosions.insert(Coord(coord.0, coord.1 + r), 1);
            self.future_explosions.insert(Coord(coord.0, coord.1 - r), 1);
          }
        },
        Some(Object::PowerUp(_)) => {
          self.visible_powerups.insert(coord);
        },
        Some(Object::Crate) => {
          self.visible_crates.insert(coord);
        },
        None => {
          self.visible_powerups.remove(&coord);
          self.visible_crates.remove(&coord);
        },
      }
      match contents.2 {
        Some(_) => {
          self.visible_enemies.insert(coord);
        },
        None => {
          self.visible_enemies.remove(&coord);
        },
      }
    }
  }

  // Remember the terrain (tile types) we can see now.
  fn remember_terrain(
    &mut self,
    surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, TileOffset)>,
  ) {
    for contents in surroundings {
      self.remember_tile(contents.0, contents.3);
    }
  }

  // Remember the type for a single tile location.
  fn remember_tile(&mut self, tile: Tile, offset: TileOffset) {
    let coord = self.get_coord(offset);
    if self.seen_terrain.insert(coord, tile) != None {
      // Already remembered before, update but exit early.
      return;
    }
    self.frontier.remove(&coord);
    // Record the first hill we see.
    if tile == Tile::Hill {
      self.known_hill = Some(coord);
    }
    // For a reachable tile, record unobserved neighbors as frontier area.
    if tile != Tile::Wall {
      for neighbor in neighbors(&coord) {
        if !(self.seen_terrain.contains_key(&neighbor) || self.frontier.contains(&neighbor)) {
          self.frontier.insert(neighbor);
        }
      }
    }
  }

  // Breadth-first search for a given goal (hill, frontier, safety, etc)
  //   and a given traversability rule (should we pass through crates, etc).
  // If a path is found, return the next tile we should move to.
  fn next_step(
    &self,
    is_goal: GoalPredicate,
    is_step_allowed: StepAllowedPredicate,
    start: Coord,
  ) -> Option<Coord> {
    let mut pending = VecDeque::<Coord>::new();
    let mut visited = HashSet::<Coord>::new();
    let mut backward = HashMap::<Coord, Coord>::new();
    visited.insert(start);
    pending.push_back(start);
    backward.insert(start, start);
    while let Some(pos) = pending.pop_front() {
      if is_goal(&self, &pos) {
        // Found goal
        let mut pos = pos;
        loop {
          match backward.get(&pos) {
            Some(&prev) => {
              if prev == start {
                // Next step after start
                return Some(pos);
              } else {
                pos = prev;
              }
            },
            None => {
              return None;
            },
          }
        }
      } else if is_step_allowed(&self, &pos) {
        // Check neighbors
        for neighbor in neighbors(&pos) {
          if !visited.contains(&neighbor) {
            visited.insert(neighbor);
            backward.insert(neighbor, pos);
            pending.push_back(neighbor);
          }
        }
      }
    }
    None
  }

  // Find an unobstructed path to the hill and return the next step we should take.
  fn next_step_to_hill(&self, start: Coord) -> Option<Coord> {
    if self.known_hill == None {
      return None;
    }
    self.next_step(
      |s, pos| -> bool { s.seen_terrain.get(&pos) == Some(&Tile::Hill) },
      |s, pos| -> bool { s.is_walkable(*pos) },
      start,
    )
  }

  // Find a (possibly obstructed) path to the hill and return the next step we should take.
  fn next_step_to_hill_through_crates(&self, start: Coord) -> Option<Coord> {
    if self.known_hill == None {
      return None;
    }
    self.next_step(
      |s, pos| -> bool { s.seen_terrain.get(&pos) == Some(&Tile::Hill) },
      |s, pos| -> bool { s.seen_terrain.get(&pos) != Some(&Tile::Wall) },
      start,
    )
  }

  // Find a (possibly obstructed) path to the frontier and return the next step we should take.
  fn next_step_to_frontier(&self, start: Coord) -> Option<Coord> {
    if self.frontier.is_empty() {
      return None;
    }
    self.next_step(
      |s, pos| -> bool { s.frontier.contains(&pos) },
      |s, pos| -> bool { s.seen_terrain.get(&pos) != Some(&Tile::Wall) },
      start,
    )
  }

  // Find an unobstructed path to safety (a tile with no pending explosions) and return the next step we should take.
  fn next_step_to_safety(&self, start: Coord) -> Option<Coord> {
    if self.future_explosions.is_empty() {
      return Some(start);
    }
    self.next_step(
      |s, &pos| -> bool { s.is_walkable(pos) && s.is_safe(pos) },
      |s, &pos| -> bool { s.is_walkable(pos) },
      start,
    )
  }

  // We want to move but don't know if it's safe yet.
  // Check for hazards and make a final decision on the action to take.
  fn consider_moving(&mut self, next: Coord) -> Action {
    // If our destination is unsafe, do something else.
    if !self.is_safe(next) {
      if self.is_safe(self.curr_abs_pos) {
        // Stay where we're at.
        return Action::StayStill;
      }
      if let Some(safety) = self.next_step_to_safety(self.curr_abs_pos) {
        // Flee.
        return Action::Move(self.step_and_record(safety));
      }
      // Couldn't find safety. Give up.
      return Action::DropBomb;
    }

    // We wanted to stay put. Probably already on a hill.
    if next == self.curr_abs_pos {
      return Action::StayStill;
    }

    if !self.is_walkable(next) {
      // Path is obstructed.
      if !self.is_safe(self.curr_abs_pos) {
        // We're in danger. Dodge if possible.
        if let Some(safety) = self.next_step_to_safety(self.curr_abs_pos) {
          // Flee.
          return Action::Move(self.step_and_record(safety));
        }
        // Couldn't find safety. Give up.
        return Action::DropBomb;
      }

      // We need to move through a crate or enemy. Let's explode it.
      for r in 0..=5 {
        self.future_explosions.insert(Coord(next.0 + r, next.1), 2);
        self.future_explosions.insert(Coord(next.0 - r, next.1), 2);
        self.future_explosions.insert(Coord(next.0, next.1 + r), 2);
        self.future_explosions.insert(Coord(next.0, next.1 - r), 2);
      }
      if let Some(safety) = self.next_step_to_safety(self.curr_abs_pos) {
        // Found a way out of our explosion. Go there.
        return Action::DropBombAndMove(self.step_and_record(safety));
      }
      // Our bomb would be suicidal. Normally, wait or find another path.
      // But since everyone is dying constantly in practice, DIE IN THE NAME OF PROGRESS
      return Action::DropBomb;
    }

    // All clear. Move normally.
    return Action::Move(self.step_and_record(next));
  }

  // Change our destination's absolute position into our direction to move.
  // Update our cached position so we know where we are next time.
  // If we ever fail at moving, bad data results. Do our best not to let that happen.
  fn step_and_record(&mut self, next: Coord) -> Direction {
    let dir = match self.curr_abs_pos.0.cmp(&next.0) {
      Ordering::Less => Direction::East,
      Ordering::Greater => Direction::West,
      Ordering::Equal => match self.curr_abs_pos.1.cmp(&next.1) {
        Ordering::Less => Direction::North,
        Ordering::Greater => Direction::South,
        Ordering::Equal => panic!("Tried to walk to same tile"),
      },
    };
    self.curr_abs_pos = next;
    dir
  }

  // Is the given coordinate a spot we can move to?
  fn is_walkable(&self, next: Coord) -> bool {
    self.seen_terrain.get(&next) != Some(&Tile::Wall)
      && !self.visible_crates.contains(&next)
      && !self.visible_enemies.contains(&next)
  }

  // Is the given coordinate safe from explosions?
  fn is_safe(&self, next: Coord) -> bool {
    // Super dumb for now, no time to deal with fuse length
    !self.future_explosions.contains_key(&next)
  }
}

// Get the four neighbors for the given coordinate.
fn neighbors(pos: &Coord) -> [Coord; 4] {
  [
    Coord(pos.0 + 1, pos.1),
    Coord(pos.0, pos.1 - 1),
    Coord(pos.0 - 1, pos.1),
    Coord(pos.0, pos.1 + 1),
  ]
}

#[cfg(test)]
mod tests {
  use super::*;
  use bomber_lib::world::Ticks;

  #[test]
  fn empty_world() {
    let mut player = MyPlayer::default();
    assert_eq!(player.act(vec![]), Action::StayStill);
  }

  #[test]
  fn closed_floor() {
    let mut player = MyPlayer::default();
    let world = vec![
      (Tile::Floor, None, None, TileOffset(0, 0)),
      (Tile::Wall, None, None, TileOffset(1, 1)),
      (Tile::Wall, None, None, TileOffset(1, 0)),
      (Tile::Wall, None, None, TileOffset(1, -1)),
      (Tile::Wall, None, None, TileOffset(0, -1)),
      (Tile::Wall, None, None, TileOffset(-1, -1)),
      (Tile::Wall, None, None, TileOffset(-1, 0)),
      (Tile::Wall, None, None, TileOffset(-1, 1)),
      (Tile::Wall, None, None, TileOffset(0, 1)),
    ];
    assert_eq!(player.act(world), Action::StayStill);
  }

  #[test]
  fn closed_hill() {
    let mut player = MyPlayer::default();
    let world = vec![
      (Tile::Hill, None, None, TileOffset(0, 0)),
      (Tile::Wall, None, None, TileOffset(1, 1)),
      (Tile::Wall, None, None, TileOffset(1, 0)),
      (Tile::Wall, None, None, TileOffset(1, -1)),
      (Tile::Wall, None, None, TileOffset(0, -1)),
      (Tile::Wall, None, None, TileOffset(-1, -1)),
      (Tile::Wall, None, None, TileOffset(-1, 0)),
      (Tile::Wall, None, None, TileOffset(-1, 1)),
      (Tile::Wall, None, None, TileOffset(0, 1)),
    ];
    assert_eq!(player.act(world), Action::StayStill);
  }

  #[test]
  fn adj_to_hill() {
    let mut player = MyPlayer::default();
    let world = vec![
      (Tile::Floor, None, None, TileOffset(0, 0)),
      (Tile::Hill, None, None, TileOffset(1, 0)),
      (Tile::Wall, None, None, TileOffset(-1, 1)),
      (Tile::Wall, None, None, TileOffset(0, 1)),
      (Tile::Wall, None, None, TileOffset(1, 1)),
      (Tile::Wall, None, None, TileOffset(2, 1)),
      (Tile::Wall, None, None, TileOffset(2, 0)),
      (Tile::Wall, None, None, TileOffset(2, -1)),
      (Tile::Wall, None, None, TileOffset(2, -1)),
      (Tile::Wall, None, None, TileOffset(1, -1)),
      (Tile::Wall, None, None, TileOffset(0, -1)),
      (Tile::Wall, None, None, TileOffset(-1, -1)),
      (Tile::Wall, None, None, TileOffset(-1, 0)),
    ];
    assert_eq!(player.act(world), Action::Move(Direction::East));
  }

  #[test]
  fn adj_to_frontier() {
    let mut player = MyPlayer::default();
    let world = vec![
      (Tile::Floor, None, None, TileOffset(0, 0)),
      (Tile::Floor, None, None, TileOffset(1, 0)),
      (Tile::Wall, None, None, TileOffset(-1, 1)),
      (Tile::Wall, None, None, TileOffset(0, 1)),
      (Tile::Wall, None, None, TileOffset(1, 1)),
      (Tile::Wall, None, None, TileOffset(1, -1)),
      (Tile::Wall, None, None, TileOffset(0, -1)),
      (Tile::Wall, None, None, TileOffset(-1, -1)),
      (Tile::Wall, None, None, TileOffset(-1, 0)),
    ];
    assert_eq!(player.act(world), Action::Move(Direction::East));
  }

  #[test]
  fn avoid_bomb() {
    let mut player = MyPlayer::default();
    let world = vec![
      (Tile::Floor, None, None, TileOffset(0, 0)),
      (
        Tile::Floor,
        Some(Object::Bomb { fuse_remaining: Ticks(1), range: 2 }),
        None,
        TileOffset(1, 0),
      ),
      (Tile::Floor, None, None, TileOffset(0, 1)),
      (Tile::Floor, None, None, TileOffset(1, 1)),
      (Tile::Wall, None, None, TileOffset(-1, 0)),
      (Tile::Wall, None, None, TileOffset(-1, 1)),
      (Tile::Wall, None, None, TileOffset(-1, 2)),
      (Tile::Wall, None, None, TileOffset(0, 2)),
      (Tile::Wall, None, None, TileOffset(1, 2)),
      (Tile::Wall, None, None, TileOffset(2, 2)),
      (Tile::Wall, None, None, TileOffset(2, 1)),
      (Tile::Wall, None, None, TileOffset(2, 0)),
      (Tile::Wall, None, None, TileOffset(2, -1)),
      (Tile::Wall, None, None, TileOffset(1, -1)),
      (Tile::Wall, None, None, TileOffset(0, -1)),
      (Tile::Wall, None, None, TileOffset(-1, -1)),
    ];
    assert_eq!(player.act(world), Action::Move(Direction::North));
  }
}
