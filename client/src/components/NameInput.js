import React from 'react';
import { compose, withState, withHandlers } from 'recompose';
import { withRouter } from 'react-router';

import localforage from 'localforage';
import socket from '../api/socket';

const enhance = compose(
  // this is so we can access props.router down below
  withRouter,

  withState('name', 'setName', ''),

  // on submit we set the name, and redirect back to the lobby
  withHandlers({
    onNameChange: props => event => {
      props.setName(event.target.value);
    },
    onSubmit: props => event => {
      event.preventDefault();

      socket.emit('SetName', props.name);

      localforage.setItem('name', props.name)
        .then(() => {
          props.router.push({
            pathname: '/lobby'
          });
        });
    }
  })
);

const NameInput = enhance(({ name, onNameChange, onSubmit }) => (
  <form onSubmit={onSubmit}>
    <div className='form-group row'>
      <label htmlFor='name'>Name</label>
      <input name='name' className='form-control' type='text' value={name} onChange={onNameChange}/>
    </div>
    <button type='submit' className='btn btn-primary'>Submit</button>
  </form>
));

export default NameInput;
