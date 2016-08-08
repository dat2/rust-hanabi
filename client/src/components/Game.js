import React from 'react';
import { compose, lifecycle } from 'recompose';
import socket from '../api/socket';

import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';
import { goBack } from 'react-router-redux';

const enhance = compose(
  // go back is for the router to go back
  connect(undefined, dispatch => bindActionCreators({ goBack }, dispatch)),

  // automatically join the game when mounted
  // automatically leave the game when unmounted
  lifecycle({
    componentDidMount() {
      const { params: { name } } = this.props;
      socket.emit('JoinChannel', name);

      socket.emit('SendMessage', 'TEST TEST 123');
      socket.on('SendMessage', (data) => {
        console.log(data);
      });
    },

    componentWillUnmount() {
      socket.emit('LeaveChannel');
    }
  })
);

const Game = enhance(({ params, goBack }) => (
  <div className='container is-fluid'>
    <button className='button' onClick={goBack}>Leave</button>
    { JSON.stringify(params) }
  </div>
));

export default Game;
