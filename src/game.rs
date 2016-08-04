use phf;
use std::cell::Cell;
use chrono::*;
use rand::{thread_rng, Rng};
use std::collections::{BTreeMap, HashMap};

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

static CARD_DISTRIBUTION: phf::Map<u64, u64> = phf_map! {
  1u64 => 3,
  2u64 => 2,
  3u64 => 2,
  4u64 => 2,
  5u64 => 1,
};

impl GameState
{
  pub fn new() -> GameState
  {
    let mut g = GameState { deck: Vec::new(), hands: BTreeMap::new(), current_turn: 0 };
    g.gen_deck();
    g.shuffle_deck();
    g
  }

  fn gen_deck(&mut self)
  {
    for x in 1..5+1
    {
      for _ in 1..CARD_DISTRIBUTION.get(&x).unwrap()+1
      {
        self.deck.push(Card { colour: Colour::White, number: x });
        self.deck.push(Card { colour: Colour::Yellow, number: x });
        self.deck.push(Card { colour: Colour::Green, number: x });
        self.deck.push(Card { colour: Colour::Red, number: x });
        self.deck.push(Card { colour: Colour::Blue, number: x });
      }
    }
  }

  // knuth shuffle
  fn shuffle_deck(&mut self)
  {
    let mut rng = thread_rng();
    rng.shuffle(&mut self.deck);
  }

  fn init_players(&mut self, players: Vec<&PlayerId>)
  {
    for id in players
    {
      self.hands.insert(id.to_string(), Vec::new());
    }
  }

  fn deal_cards(&mut self, n_cards: usize)
  {
    let keys = self.hands.keys().cloned().collect::<Vec<_>>();

    for key in keys.iter().cycle().take(keys.len() * n_cards)
    {
      self.hands.get_mut(key).unwrap().push(self.deck.pop().unwrap());
    }
  }
}

pub type ChannelName = String;

#[derive(Debug)]
pub struct Player
{
  name: String
}

#[derive(Debug)]
pub struct Message
{
  text: String,
  timestamp: DateTime<UTC>
}

#[derive(Debug)]
pub struct Channel
{
  pub next_player_id: Cell<PlayerId>,
  pub players: HashMap<PlayerId, Player>,
  pub chat: Vec<Message>,
  pub game_state: GameState
}

impl Channel
{
  pub fn new() -> Channel
  {
    Channel
    {
      next_player_id: Cell::new(0),
      players: HashMap::new(),
      chat: Vec::new(),
      game_state: GameState::new()
    }
  }

  pub fn has_player_with_name(&self, name: String) -> bool
  {
    self.players.values().fold(false, |found, player| found || player.name == name)
  }

  pub fn add_player(&mut self, name: String)
  {
    // TODO generate JSON web token?
    self.players.insert(self.next_player_id.get(), Player { name: name });
    self.next_player_id.set(self.next_player_id.get() + 1);
  }

  // TODO remove player?

  pub fn add_message(&mut self, text: String)
  {
    self.chat.push(Message { text: text, timestamp: UTC::now() });
  }

  pub fn start_game(&mut self)
  {
    let player_ids = self.players.keys().collect::<Vec<_>>();
    self.game_state.init_players(player_ids);
    // deal 4 cards to each player
    self.game_state.deal_cards(4);
  }
}
