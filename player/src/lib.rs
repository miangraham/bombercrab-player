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
}

#[wasm_export]
impl Player for MyPlayer {
  fn act(
    &mut self,
    surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, TileOffset)>,
  ) -> Action {
    self.remember_terrain(surroundings);

    if self.known_hill != None && let Some(next) = self.next_step_to_hill(self.curr_abs_pos) {
      if next == self.curr_abs_pos {
        // King of the hill
        return Action::StayStill;
      }
      // Take known path to hill
      return Action::Move(self.step_and_record(next));
    }

    if let Some(next) = self.next_step_to_frontier(self.curr_abs_pos) {
      // No known path to hill, explore frontier
      return Action::Move(self.step_and_record(next));
    }

    // No reachable frontier. Give up.
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
      // Check neighbors
      for neighbor in neighbors(&pos) {
        if !visited.contains(&neighbor) {
          visited.insert(neighbor);
          backward.insert(neighbor, pos);
          pending.push_back(neighbor);
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

      // Check neighbors
      for neighbor in neighbors(&pos) {
        if !visited.contains(&neighbor) {
          visited.insert(neighbor);
          backward.insert(neighbor, pos);
          pending.push_back(neighbor);
        }
      }
    }
    None
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
