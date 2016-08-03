#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]
#![plugin(dotenv_macros)]

// import external
extern crate serde;
extern crate serde_json;
extern crate ws;
extern crate dotenv;
extern crate rand;

// import local
mod messages;

use dotenv::dotenv;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use ws::{listen,Message,Sender};
use serde_json::{Value, Map, to_value};
use messages::{JsonMessage,Event};
use rand::{thread_rng, Rng};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
enum Colour
{
  White,
  Yellow,
  Green,
  Red,
  Blue
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Card
{
  colour: Colour,
  number: u64
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct GameState
{
  deck: Vec<Card>
}

impl GameState
{
  fn new(max_num: u64) -> GameState
  {
    let mut g = GameState { deck: Vec::new() };
    g.gen_deck(max_num);
    g.shuffle_deck();
    g
  }

  fn gen_deck(&mut self, max_num: u64)
  {
    for x in 1..max_num+1 {
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
}

type PlayerId = u64;

// https://github.com/housleyjk/ws-rs/issues/56#issuecomment-231497839
fn main()
{
  dotenv().ok();

  let registry: Registry = Rc::new(RefCell::new(HashMap::new()));
  let mut id_counter = 0;

  // the main server handler
  listen(dotenv!("BIND"), |out|
  {
    id_counter = id_counter + 1;

    Handler
    {
      id: id_counter,
      out: Rc::new(out),
      registry: registry.clone(),
    }
  }).unwrap()
}

// cases we need to handle
// intermittent client connections

// return a string to send back to the client
fn handle_init(_: &Value, out: Rc<Sender>) -> Result<(), ws::Error>
{
  let game = GameState::new(5);
  let response = JsonMessage { event: Event::Init, data: to_value(&game) };

  // return the string to send
  let response = serde_json::to_string(&response).unwrap();
  out.send(response)
}

type ChannelName = String;
type Registry = Rc<RefCell<HashMap<u64, Rc<Sender>>>>;

struct Handler
{
  id: u64,
  out: Rc<Sender>,
  registry: Registry
}

impl ws::Handler for Handler {
  fn on_open(&mut self, _: ws::Handshake) -> ws::Result<()>
  {
    self.registry.borrow_mut().insert(self.id, self.out.clone());
    Ok(())
  }

  fn on_message(&mut self, msg: Message) -> ws::Result<()>
  {
    // parse the received message
    let json_string = msg.as_text().unwrap();

    // try unwrapping if possible, else print an error
    let message: JsonMessage = match serde_json::from_str(&json_string) {
      Ok(m) => m,

      // print an error
      Err(e) => {
        println!("The message received is not valid {:?}", e);
        return Ok(());
      }
    };

    println!("Message received {:?}", message);

    match message.event {
      Event::Init => handle_init(&message.data, self.out.clone())
    }
  }

  fn on_close(&mut self, _: ws::CloseCode, _: &str)
  {
    self.registry.borrow_mut().remove(&self.id);
  }
}
