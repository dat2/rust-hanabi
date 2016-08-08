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
use std::rc::{Rc,Weak};
use std::cell::RefCell;
use std::collections::HashMap;
use ws::{listen,Message,Sender};

// import local
mod messages;
mod game;
use game::{Channel};
use messages::{JsonMessage,Event};

type Registry = Rc<RefCell<HashMap<u64, Rc<Sender>>>>;
type ChannelRegistry = Rc<RefCell<HashMap<String,Rc<RefCell<Channel>>>>>;

struct Handler
{
  id: u64,
  name: String,
  out: Rc<Sender>,
  registry: Registry,
  channels: ChannelRegistry,
  current_channel: Option<Rc<RefCell<Channel>>>
}

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
      current_channel: None
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

fn handle_get_channels(handler: &Handler) -> Result<(), ws::Error>
{
  let keys = Vec::from_iter(handler.channels.borrow().keys().map(|s| s.to_owned()));
  let response = JsonMessage { payload: Event::SendChannels(keys) };
  serialize_response(response, handler.out.clone())
}

fn handle_create_channel(handler: &Handler, name: String) -> Result<(), ws::Error>
{
  let mut borrowed = handler.channels.borrow_mut();
  if borrowed.contains_key(&name)
  {
    let response = JsonMessage { payload: Event::Error(format!("The channel {} already exists!", name)) };
    serialize_response(response, handler.out.clone())
  }
  else
  {
    borrowed.insert(name, Rc::new(RefCell::new(Channel::new())));
    Ok(())
  }
}

fn handle_set_name(handler: &mut Handler, new_name: String) -> Result<(), ws::Error>
{
  handler.name = new_name;
  Ok(())
}

fn handle_join_channel(handler: &mut Handler, channel_name: String) -> Result<(), ws::Error>
{
  let mut borrowed = handler.channels.borrow_mut();
  match borrowed.get_mut(&channel_name)
  {
    Some(chan) =>
    {
      chan.borrow_mut().add_player(&handler.name);
      handler.current_channel = Some(chan.clone());
      Ok(())
    },
    None =>
    {
      let response = JsonMessage { payload: Event::Error(format!("The channel {} doesn't exist", channel_name)) };
      serialize_response(response, handler.out.clone())
    }
  }
}

fn handle_send_message(handler: &mut Handler, message: String) -> Result<(), ws::Error>
{
  match handler.current_channel
  {
    Some(chan) =>
    {
      Ok(())
    },
    None =>
    {
      let response = JsonMessage { payload: Event::Error(String::from("You are not in a channel!")) };
      serialize_response(response, handler.out.clone())
    }
  }
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
    let message: JsonMessage = match serde_json::from_str(json_string)
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
      Event::GetChannels => handle_get_channels(self),
      Event::CreateChannel(channel_name) => handle_create_channel(self, channel_name),
      Event::SetName(new_name) => handle_set_name(self, new_name),
      Event::JoinChannel(channel_name) => handle_join_channel(self, channel_name),
      Event::SendMessage(message) => handle_send_message(self, message),
      _ => Ok(())
    }
  }

  fn on_close(&mut self, _: ws::CloseCode, _: &str)
  {
    self.registry.borrow_mut().remove(&self.id);
  }
}
