import React from 'react';
import { compose, branch, renderComponent } from 'recompose';
import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';
import { push } from 'react-router-redux';

const Spinner = () => (
  <div className='container'>
    <div className='image is-16by9'>
      <img src='http://placehold.it/1600x900' alt='' />
    </div>
  </div>
);

const Channel = ({ name, joinChannel }) => (
  <div className='card is-fullwidth'>
    <div className='card-header'>
      <p className='card-header-title'>
        { name }
      </p>
      <a className='card-header-icon' onClick={joinChannel}>
        <i className='fa fa-sign-in' />
      </a>
    </div>

    <div className='card-content'>

    </div>

    <div className='card-footer'>
      <a className='card-footer-item' onClick={joinChannel}>
        <span className='icon'>
          <i className='fa fa-sign-in' />
        </span>
        <span>Join</span>
      </a>
    </div>
  </div>
);

// show the spinner component if we're not loaded yet
const identity = t => t;
const enhance = compose(
  connect(undefined, dispatch => bindActionCreators({ joinChannel: c => push(`/game/${c}`) }, dispatch)),

  branch(
    ({ loading }) => loading,
    renderComponent(Spinner),
    identity
  )
);
const ChannelsList = enhance(({ channels, joinChannel }) => (
  <div className='columns is-multiline'>
    {
      channels.map((name,i) =>
        <div className='column is-one-quarter' key={i}>
          <Channel name={name} joinChannel={() => joinChannel(name)} />
        </div>
      )
    }
  </div>
));

export default ChannelsList;
