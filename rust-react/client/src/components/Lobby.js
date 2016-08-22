import React from 'react';
import { compose, lifecycle, withState } from 'recompose';

// redux actions
import { bindActionCreators } from 'redux';
import { connect } from 'react-redux';
import { fetchChannels, requestFetchChannels } from '../modules/channels';
import socket from '../api/socket';

import ChannelsList from './ChannelsList';
import CreateChannelModal from './CreateChannelModal';

// Lobby : JoinChannel -> Redirect to Game
// Lobby : CreateChannel -> Redirect to CreateGame

// emit the GetChannels event, and dispatch a requested action.
const getChannels = () => (dispatch) => {
  dispatch(requestFetchChannels());
  socket.emit('GetChannels');
};

// listen for the SendChannels, Error, and error event, and dispatch
// the fetchChannels action
const listenForFetchChannels = () => (dispatch) => {
  ['SendChannels', 'Error', 'error'].forEach(listener => {
    socket.on(listener, (data) => {
      dispatch(fetchChannels(data));
    });
  });
};

const enhance = compose(
  withState('modalOpen', 'setModalOpen', false),

  // use the channels reducer
  connect( ({ channels }) => ({ channels }), dispatch => bindActionCreators({ listenForFetchChannels, getChannels }, dispatch)),

  // when the component mounts, automatically fetch the channels
  lifecycle({
    componentWillMount() {
      const { listenForFetchChannels, getChannels } = this.props;

      listenForFetchChannels();
      getChannels();
    }
  }),
);
const Lobby = enhance(({ channels, getChannels, modalOpen, setModalOpen }) => (
  <div className='is-fullwidth'>
    {/* the nav */}
    <nav className='nav has-shadow'>
      <div className='nav-left'>
        {/* the first nav button */}
        <div className='nav-item'>
          <button className={`button ${channels.loading ? 'is-loading' : ''}`} type='button' onClick={getChannels}>
            <span className='icon'>
              <i className='fa fa-refresh' />
            </span>
            <span>Reload Channels</span>
          </button>
        </div>

        {/* the second nav button */}
        <div className='nav-item'>
          <button className='button' type='button' onClick={() => setModalOpen(true)}>
            <span className='icon'>
              <i className='fa fa-plus-circle' />
            </span>
            <span>Create new Channel</span>
          </button>
        </div>
      </div>
    </nav>

    <div className='section'>
      {/* the create game modal */}
      <CreateChannelModal isOpen={modalOpen} close={() => setModalOpen(false)} />

      {/* the actual list */}
      <ChannelsList {...channels} />
    </div>
  </div>
));

export default Lobby;
