import React from 'react';
import ReactDOM, { createRoot } from 'react-dom';
import { HashRouter, Route } from 'react-router-dom';
import { AssNBalls } from './AssNBalls';
import {TitsNBalls} from './TitsNBalls';
import {CockNBalls} from './CockNBalls';

//main
function Pisshitcock() {
    return(
            <HashRouter>
                <div className="cockground">
                    <Route path="/" exact component={CockNBalls} />
                    <Route path="/cock" exact component={CockNBalls} />
                    <Route path="/ass" exact component={AssNBalls} />
                    <Route path="/titties" exact component={TitsNBalls} />
                </div>    
            </HashRouter>
    );
}

function idiot() {
    createRoot(document.getElementById('leRoot')).render(<Pisshitcock/>,null);
}

export default idiot;
