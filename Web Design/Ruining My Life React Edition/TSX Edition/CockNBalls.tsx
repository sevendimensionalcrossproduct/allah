import React from 'react';
import {Cockmaster3} from './Cockmaster3';

function CockNBalls() {
    return (
        <div>
            <Cockmaster />
            <Cockmaster2 />
            <Cockmaster3 />
        </div>
    );
}

function Cockmaster() {
    const [message, setMessage] = React.useState('cockle until u groggle');

    const toggleMessage = () => {setMessage(message === 'cockle until u groggle' ? 'cock' : 'cockle until u groggle');};

    const [count, setCount] = React.useState(0);

    const [inputValue, setInputValue] = React.useState('');
    const [text, setText] = React.useState('');

    const cockle = (event : any) => {setInputValue(event.target.value);};

    const handleClick = () => {
        setText(inputValue);
        document.title = inputValue;
        setInputValue('');
    };

    return(
        <div className="cockmaster">
            <div className="penis">
                <h1>shitty react</h1> 
                <p>Welcome to React shit edition</p>
                <p>like react but more ass</p>
                <button onClick={toggleMessage}>cock</button>
                <p className="islam">{message}</p>
            </div>

            <div className="penis">
                <p>clickety click {count} </p>
                <button onClick={() => setCount(count + 1)}>click for sex</button>
            </div>
            <div className="penis">
                <input type="text" value={inputValue} onChange={cockle} style={{ marginTop: '30px' }} onKeyUp={(e) => {if (e.key === 'Enter') {handleClick()}}}/>
                <button onClick={handleClick} style={{ marginTop: '10px' }} >Submit</button>
                <p>Your cock: {text}</p>
                <p
                style={{ marginTop: '-20px' }}
                >(also changes title!)</p>
            </div>
        </div>
    );
}

function Cockmaster2() {
    const [imageSrc, setImageSrc] = React.useState('https://i.kym-cdn.com/entries/icons/mobile/000/031/051/cover4.jpg');
    
    const colorMagic = () => {
        setImageSrc(imageSrc !== '/home/serna/vscoed/back end practice/next-attempt/ruining my life/bloggif_666c18f8e3dbd.gif' ? '/home/serna/vscoed/back end practice/next-attempt/ruining my life/bloggif_666c18f8e3dbd.gif' : 'https://i.kym-cdn.com/entries/icons/mobile/000/031/051/cover4.jpg');
    };

    const wavyMagic = () => {
        setImageSrc(imageSrc !== './cocknballs.gif' ? './cocknballs.gif' : 'https://i.kym-cdn.com/entries/icons/mobile/000/031/051/cover4.jpg');
    };

    const jeff = () => {
       setImageSrc(imageSrc !== './jeff.gif' ? './jeff.gif' : 'https://i.kym-cdn.com/entries/icons/mobile/000/031/051/cover4.jpg');
    };

    const [isShaking, setIsShaking] = React.useState(false);
    const toggleShake = () => {
        setIsShaking(!isShaking);
    };

    return (
        <div className="cockmaster2">
            <img src= {imageSrc} onClick= {toggleShake} className={isShaking ? 'shake' : 'cockimage'}>
            </img>
            <button style= {{ display: 'flex', position: 'relative', marginTop: '40px' }} onClick = {colorMagic}> magic </button>
            <button style= {{ display: 'flex', position: 'relative', marginTop: '40px' }} onClick = {wavyMagic}> magic 2 </button>
            <button style= {{ display: 'flex', position: 'relative', marginTop: '40px' }} onClick = {jeff}> jeff </button>

            <img onClick={() => alert("FUCK FRANCE")} src='https://upload.wikimedia.org/wikipedia/commons/a/a4/Animated-Flag-France.gif' style={{ position: 'relative', marginTop: '40px', height: '100px' }} />
        </div>
    );
}

export {CockNBalls};
