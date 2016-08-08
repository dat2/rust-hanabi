import React from 'react';
import { compose, withState, withHandlers } from 'recompose';

import socket from '../api/socket';

const enhance = compose(
  withState('name', 'setName', ''),

  withHandlers({
    onNameChange: props => event => {
      props.setName(event.target.value);
    },

    onSubmit: props => event => {
      event.preventDefault();

      socket.emit('CreateChannel', props.name);
      socket.emit('JoinChannel', props.name);
      socket.emit('GetChannels');

      props.close();
      props.setName('');
    }
  })
);

const CreateGameModal = enhance(({ isOpen, close, name, onNameChange, onSubmit }) => (
  <div className={`modal ${isOpen ? 'is-active': ''}`}>
    <div className='modal-background' onClick={close}></div>
    <div className='modal-card'>
      <header className='modal-card-head'>
        <p className='modal-card-title'>Create channel</p>
        <button className='delete'  onClick={close}></button>
      </header>
      <section className='modal-card-body'>

        <form onSubmit={onSubmit}>
          <p className='control'>
            <label className='label'>Channel name</label>
            <input className='input' type='text' value={name} onChange={onNameChange}/>
          </p>
        </form>

      </section>
      <footer className='modal-card-foot'>
        <a className='button is-primary' onClick={onSubmit}>Create Channel</a>
        <a className='button' onClick={close}>Cancel</a>
      </footer>
    </div>
  </div>
));

export default CreateGameModal;
