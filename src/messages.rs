use game::{GameState};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Event
{
  Init,
  GetChannels,
  CreateChannel(String),
  JoinChannel(String),
  LeaveChannel,
  StartGame,
  // The rest here is for us sending to the client, and we will never receive it
  SendChannels(Vec<String>),
  SendState(GameState)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct JsonMessage
{
  pub data: Event
}
