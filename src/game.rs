
use rand::{thread_rng, Rng};
use std::collections::BTreeMap;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Colour
{
  White,
  Yellow,
  Green,
  Red,
  Blue
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Card
{
  colour: Colour,
  number: u64
}

pub type PlayerId = u64;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct GameState
{
  pub deck: Vec<Card>,
  pub hands: BTreeMap<String, Vec<Card>>,
  pub current_turn: PlayerId
}

impl GameState
{
  pub fn new(max_card_num: u64, n_players: u64) -> GameState
  {
    let mut g = GameState { deck: Vec::new(), hands: BTreeMap::new(), current_turn: 0 };
    g.gen_deck(max_card_num);
    g.shuffle_deck();
    g.init_players(n_players);
    g
  }

  fn gen_deck(&mut self, max_card_num: u64)
  {
    for x in 1..max_card_num+1 {
      self.deck.push(Card { colour: Colour::White, number: x });
      self.deck.push(Card { colour: Colour::Yellow, number: x });
      self.deck.push(Card { colour: Colour::Green, number: x });
      self.deck.push(Card { colour: Colour::Red, number: x });
      self.deck.push(Card { colour: Colour::Blue, number: x });
    }
  }

  // knuth shuffle
  fn shuffle_deck(&mut self)
  {
    let mut rng = thread_rng();
    rng.shuffle(&mut self.deck);
  }

  fn init_players(&mut self, n_players: u64)
  {
    for p in 0..n_players {
      self.hands.insert(p.to_string(), Vec::new());
    }
  }
}
