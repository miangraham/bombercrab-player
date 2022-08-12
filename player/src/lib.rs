use bomber_lib::{
  self,
  world::{Direction, Enemy, Object, Tile, TileOffset},
  Action, Player,
};
use bomber_macro::wasm_export;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(
  Copy, Clone, Debug, Default, Eq, PartialEq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Coord(pub i32, pub i32);

/// Player struct. Can contain any arbitrary data, which will carry over between turns.
#[derive(Default)]
struct MyPlayer {
  curr_abs_pos: Coord,
  seen_terrain: HashMap<Coord, Tile>,
  known_hill: Option<Coord>,
  frontier: HashSet<Coord>,

  visible_crates: HashSet<Coord>,
  visible_enemies: HashSet<Coord>,
  visible_powerups: HashSet<Coord>,
  future_explosions: HashMap<Coord, u32>,
}

#[wasm_export]
impl Player for MyPlayer {
  fn act(
    &mut self,
    surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, TileOffset)>,
  ) -> Action {
    self.remember_terrain(surroundings.clone());
    self.remember_objects(surroundings);

    if self.known_hill != None && let Some(next) = self.next_step_to_hill(self.curr_abs_pos) {
      // Move towards hill
      return self.consider_moving(next);
    }

    if let Some(next) = self.next_step_to_frontier(self.curr_abs_pos) {
      // No known path to hill, explore frontier
      return self.consider_moving(next);
    }

    // No reachable frontier. Give up.
    // XXX Flee when necessary
    Action::StayStill
  }

  fn name(&self) -> String {
    String::from("sometypes")
  }

  fn team_name() -> String {
    String::from("bocchi")
  }
}

impl MyPlayer {
  fn get_coord(&self, offset: TileOffset) -> Coord {
    Coord(offset.0 + self.curr_abs_pos.0, offset.1 + self.curr_abs_pos.1)
  }

  fn remember_objects(
    &mut self,
    surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, TileOffset)>,
  ) {
    self.future_explosions.clear();

    for contents in surroundings {
      let coord = self.get_coord(contents.3);
      match contents.1 {
        Some(Object::Bomb { fuse_remaining: _, range }) => {
          // do something dumb for now
          for r in 0..=range {
            let r = r as i32;
            self.future_explosions.insert(Coord(r, 0), 1);
            self.future_explosions.insert(Coord(-r, 0), 1);
            self.future_explosions.insert(Coord(0, r), 1);
            self.future_explosions.insert(Coord(0, -r), 1);
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

  fn remember_terrain(
    &mut self,
    surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, TileOffset)>,
  ) {
    for contents in surroundings {
      self.remember_tile(contents.0, contents.3);
    }
  }

  fn remember_tile(&mut self, tile: Tile, offset: TileOffset) {
    let coord = self.get_coord(offset);
    if self.seen_terrain.insert(coord, tile) != None {
      // Already remembered, assume no changes
      return;
    }
    self.frontier.remove(&coord);
    if tile == Tile::Hill {
      self.known_hill = Some(coord);
    }
    if tile != Tile::Wall {
      for neighbor in neighbors(&coord) {
        if !(self.seen_terrain.contains_key(&neighbor) || self.frontier.contains(&neighbor)) {
          self.frontier.insert(neighbor);
        }
      }
    }
  }

  fn next_step_to_hill(&self, start: Coord) -> Option<Coord> {
    if self.known_hill == None {
      return None;
    }
    let mut pending = VecDeque::<Coord>::new();
    let mut visited = HashSet::<Coord>::new();
    let mut backward = HashMap::<Coord, Coord>::new();
    visited.insert(start);
    pending.push_back(start);
    backward.insert(start, start);
    while let Some(pos) = pending.pop_front() {
      if self.seen_terrain.get(&pos) == Some(&Tile::Hill) {
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
      }
      if self.seen_terrain.get(&pos) == Some(&Tile::Floor) {
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

  fn next_step_to_frontier(&self, start: Coord) -> Option<Coord> {
    if self.frontier.is_empty() {
      return None;
    }
    let mut pending = VecDeque::<Coord>::new();
    let mut visited = HashSet::<Coord>::new();
    let mut backward = HashMap::<Coord, Coord>::new();
    visited.insert(start);
    pending.push_back(start);
    backward.insert(start, start);
    while let Some(pos) = pending.pop_front() {
      if self.frontier.contains(&pos) {
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
      }

      if self.seen_terrain.get(&pos) == Some(&Tile::Floor) {
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

  fn next_step_to_safety(&self, start: Coord) -> Option<Coord> {
    if self.future_explosions.is_empty() {
      return Some(start);
    }
    let mut pending = VecDeque::<Coord>::new();
    let mut visited = HashSet::<Coord>::new();
    let mut backward = HashMap::<Coord, Coord>::new();
    visited.insert(start);
    pending.push_back(start);
    backward.insert(start, start);
    while let Some(pos) = pending.pop_front() {
      if self.is_safe(pos) {
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
      }

      if self.seen_terrain.get(&pos) == Some(&Tile::Floor) {
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

  fn consider_moving(&mut self, next: Coord) -> Action {
    if !self.is_safe(next) {
      // Danger danger
      if self.is_safe(self.curr_abs_pos) {
        // Wait
        return Action::StayStill;
      }
      if let Some(safety) = self.next_step_to_safety(self.curr_abs_pos) {
        // Flee
        return Action::Move(self.step_and_record(safety));
      }
      // Couldn't find safety. Give up.
      return Action::StayStill;
    }
    if next == self.curr_abs_pos {
      return Action::StayStill;
    }

    if self.visible_crates.contains(&next) || self.visible_enemies.contains(&next) {
      // Blocked, let's see if we can explode it
      for r in 0..=5 {
        self.future_explosions.insert(Coord(r, 0), 2);
        self.future_explosions.insert(Coord(-r, 0), 2);
        self.future_explosions.insert(Coord(0, r), 2);
        self.future_explosions.insert(Coord(0, -r), 2);
      }
      if let Some(safety) = self.next_step_to_safety(self.curr_abs_pos) {
        return Action::DropBombAndMove(self.step_and_record(safety));
      }
      // Bomb would be suicidal, wait a tick
      return Action::StayStill;
    }

    return Action::Move(self.step_and_record(next));
  }

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

  fn is_safe(&self, next: Coord) -> bool {
    // dumb for now
    if self.future_explosions.contains_key(&next) {
      false
    } else {
      true
    }
  }
}

fn neighbors(pos: &Coord) -> [Coord; 4] {
  [
    Coord(pos.0 + 1, pos.1),
    Coord(pos.0 - 1, pos.1),
    Coord(pos.0, pos.1 - 1),
    Coord(pos.0, pos.1 + 1),
  ]
}

#[cfg(test)]
mod tests {
  use super::*;

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
}
