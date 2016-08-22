import React from 'react';
import { Provider } from 'react-redux';
import { Router } from 'react-router';
import Router from '../components/Router';
import makeRoutes from '../routes';

const Root = ({ store, history }) => (
  <Provider store={store}>
    <div className='root'>
      <Router history={history} routes={makeRoutes(store)}/>
    </div>
  </Provider>
);

export default Root;
