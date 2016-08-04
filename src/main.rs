#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]
#![plugin(dotenv_macros)]
#![plugin(phf_macros)]

// import external
extern crate serde;
extern crate serde_json;
extern crate ws;
extern crate dotenv;
extern crate rand;
extern crate chrono;
#[macro_use]
extern crate phf;

use std::iter::FromIterator;
use dotenv::dotenv;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use ws::{listen,Message,Sender};

// import local
mod messages;
mod game;
use game::{Channel};
use messages::{JsonMessage,Event};

type Registry = Rc<RefCell<HashMap<u64, Rc<Sender>>>>;
type ChannelRegistry = Rc<RefCell<HashMap<String,Channel>>>;

// https://github.com/housleyjk/ws-rs/issues/56#issuecomment-231497839
fn main()
{
  dotenv().ok();

  let registry: Registry = Rc::new(RefCell::new(HashMap::new()));
  let channels: ChannelRegistry = Rc::new(RefCell::new(HashMap::new()));
  let mut id_counter = 0;

  // the main server handler
  listen(dotenv!("BIND"), |out|
  {
    id_counter += 1;

    Handler
    {
      id: id_counter,
      name: String::new(),
      out: Rc::new(out),
      registry: registry.clone(),
      channels: channels.clone(),
    }
  }).unwrap()
}

// cases we need to handle
// intermittent client connections

fn serialize_response<T> (response: T, out: Rc<Sender>) -> Result<(), ws::Error>
  where T: serde::Serialize
{
  match serde_json::to_string(&response)
  {
    Ok(response) =>
    {
      println!("Sending {:?}", response);
      out.send(response)
    },
    Err(e) =>
    {
      println!("Error while serializing response, {:?}", e);
      Ok(())
    }
  }
}

fn handle_get_channels(channels: ChannelRegistry, out: Rc<Sender>) -> Result<(), ws::Error>
{
  let keys = Vec::from_iter(channels.borrow().keys().map(|s| s.to_owned()));
  let response = JsonMessage { payload: Event::SendChannels(keys) };
  serialize_response(response, out)
}

fn handle_create_channel(channels: ChannelRegistry, name: String, out: Rc<Sender>) -> Result<(), ws::Error>
{
  let mut borrowed = channels.borrow_mut();
  if borrowed.contains_key(&name)
  {
    let response = JsonMessage { payload: Event::Error(format!("The channel {} already exists!", name)) };
    serialize_response(response, out)
  }
  else
  {
    borrowed.insert(name, Channel::new());
    Ok(())
  }
}

fn handle_set_name(handler_name: &mut String, new_name: String) -> Result<(), ws::Error>
{
  *handler_name = new_name;
  Ok(())
}

fn handle_join_channel(channels: ChannelRegistry, player_name: &String, channel_name: String, out: Rc<Sender>) -> Result<(), ws::Error>
{
  let mut borrowed = channels.borrow_mut();
  if let Some(chan) = borrowed.get_mut(&channel_name)
  {
    chan.add_player(player_name);
    Ok(())
  }
  else
  {
    let response = JsonMessage { payload: Event::Error(format!("The channel {} doesn't exist", channel_name)) };
    serialize_response(response, out)
  }
}

struct Handler
{
  id: u64,
  name: String,
  out: Rc<Sender>,
  registry: Registry,
  channels: ChannelRegistry
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
    let message: JsonMessage = match serde_json::from_str(&json_string)
    {
      Ok(m) => m,

      // print an error
      Err(e) =>
      {
        println!("The message received is not valid {:?}", e);
        return Ok(());
      }
    };

    println!("Received {:?}", message);

    match message.payload
    {
      Event::GetChannels => handle_get_channels(self.channels.clone(), self.out.clone()),
      Event::CreateChannel(channel_name) => handle_create_channel(self.channels.clone(), channel_name, self.out.clone()),
      Event::SetName(new_name) => handle_set_name(&mut self.name, new_name),
      Event::JoinChannel(channel_name) => handle_join_channel(self.channels.clone(), &self.name, channel_name, self.out.clone()),
      _ => Ok(())
    }
  }

  fn on_close(&mut self, _: ws::CloseCode, _: &str)
  {
    self.registry.borrow_mut().remove(&self.id);
  }
}
