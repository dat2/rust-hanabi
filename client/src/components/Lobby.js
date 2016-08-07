import React from 'react';

import { compose, lifecycle, withReducer, branch, renderComponent } from 'recompose';
import channelsReducer, { createFetchChannelsAction, createFetchChannelsRequestedAction, initialState } from '../modules/channels';
import socket from '../api/socket';

const identity = t => t;

// Lobby : JoinChannel -> Redirect to Game
// Lobby : CreateChannel -> Redirect to CreateGame

// emit the GetChannels event, and dispatch a requested action.
const fetchChannels = (dispatch) => {
  dispatch(createFetchChannelsRequestedAction());
  socket.emit('GetChannels');
};

// listen for the SendChannels, Error, and error event, and dispatch
// the fetchChannels action
const listenForFetchChannels = (dispatch) => {
  ['SendChannels', 'Error', 'error'].forEach(listener => {
    socket.on(listener, (data) => {
      dispatch(createFetchChannelsAction(data));
    });
  });
};

const Spinner = () => <p>Loading</p>;

const enhance = compose(
  // use the channels reducer
  withReducer('channels', 'dispatch', channelsReducer, initialState),

  // when the component mounts, automatically fetch the channels
  lifecycle({
    componentWillMount() {
      const { dispatch } = this.props;

      listenForFetchChannels(dispatch);
      fetchChannels(dispatch);
    }
  }),

  // show the spinner component if we're not loaded yet
  branch(
    ({ channels }) => !channels.loading,
    identity,
    renderComponent(Spinner)
  )
);
const Lobby = enhance(({ channels, dispatch, router }) => (
  <div>
    <button type='button' className='btn btn-primary' onClick={() => fetchChannels(dispatch)}>Reload Channels</button>
    <ul className='list-group'>
      {
        channels.channels.map((c,i) => <li key={i} className='list-group-item'>{ c }</li>)
      }
    </ul>
  </div>
));

export default Lobby;
