use bomber_lib::{
  self,
  world::{Enemy, Object, Tile},
  Action, Player,
};
use bomber_macro::wasm_export;

/// Player struct. Can contain any arbitrary data, which will carry over between turns.
#[derive(Default)]
struct MyPlayer;

#[wasm_export]
impl Player for MyPlayer {
  fn act(
    &mut self,
    _surroundings: Vec<(Tile, Option<Object>, Option<Enemy>, bomber_lib::world::TileOffset)>,
  ) -> Action {
    Action::StayStill
  }

  fn name(&self) -> String {
    String::from("wholesometypes")
  }

  fn team_name() -> String {
    String::from("bocchi")
  }
}
