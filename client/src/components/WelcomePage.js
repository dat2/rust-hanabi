import React from 'react';
import { compose, withHandlers } from 'recompose';

// redux actions
import { bindActionCreators } from 'redux';
import { connect } from 'react-redux';
import { setName } from '../modules/user';
import { push } from 'react-router-redux';

import socket from '../api/socket';

const enhance = compose(
  // inject redux setName into the props
  connect(({ user }) => ({ name: user.name }), (dispatch) => bindActionCreators({ setName, push }, dispatch)),

  // on submit we set the name, save it locally, and redirect back to the lobby
  withHandlers({
    onNameChange: props => event => {
     props.setName(event.target.value);
    },
    onSubmit: props => event => {
      event.preventDefault();

      socket.emit('SetName', props.name);
      props.push({ pathname: '/lobby' });
    }
  }),
);

const WelcomePage = enhance(({ name, onNameChange, onSubmit }) => (
  <div className='hero is-primary is-fullheight'>
    <div className='hero-body'>
      <div className='container'>
        <h1 className='title'>Welcome To Hanabi!</h1>

        <form onSubmit={onSubmit}>
          <p className='control'>
            <label className='label'>Please type in your name before you can play</label>
            <input className='input' type='text' value={name} onChange={onNameChange}/>
          </p>
          <p className='control'>
            <button className='button is-success' type='submit'>
              <span className='icon'>
                <i className='fa fa-play' />
              </span>
              <span>Let's Go!</span>
            </button>
          </p>
        </form>
      </div>
    </div>
  </div>
));

export default WelcomePage;
