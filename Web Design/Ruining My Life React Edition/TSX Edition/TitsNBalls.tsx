import React from 'react';
import {Cockmaster3} from './Cockmaster3';

function TitsNBalls() {
      interface backgroundStyle {
  background?: string;
  backgroundImage?: string;
  backgroundSize?: string;
  backgroundRepeat?: string;
  backgroundAttachment?: string;
}


      const [backgroundStyle, setBackgroundStyle] = React.useState<backgroundStyle>({});

  const setPiss = () => {
    setBackgroundStyle({
      backgroundImage: 'linear-gradient(44deg, #33ff11, #ff9944)',
      backgroundSize: 'cover',
      backgroundAttachment: 'fixed'
    });
  };

  const setKhalifa = () => {
    setBackgroundStyle({
      background: 'url(./khalifa.jpg) no-repeat center center fixed',
      backgroundSize: 'cover'
    });
  };

  const setKhalifaArmy = () => {
    setBackgroundStyle({
      background: 'url(./khalifa.jpg)',
    });
  };


React.useEffect(() => {
  const bodyElement = document.querySelector('body');
  if (bodyElement) {
    const bodyStyle = bodyElement.style;
    if (backgroundStyle.background) {
      bodyStyle.background = backgroundStyle.background;
    }
    if (backgroundStyle.backgroundImage) {
      bodyStyle.backgroundImage = backgroundStyle.backgroundImage;
    }
    if (backgroundStyle.backgroundSize) {
      bodyStyle.backgroundSize = backgroundStyle.backgroundSize;
    }
    if (backgroundStyle.backgroundRepeat) {
      bodyStyle.backgroundRepeat = backgroundStyle.backgroundRepeat;
    }
    if (backgroundStyle.backgroundAttachment) {
      bodyStyle.backgroundAttachment = backgroundStyle.backgroundAttachment;
    }
  }
}, [backgroundStyle]);



    return(
        <div>
                    <div style={{position: 'absolute', left: '333px', justifyContent:'center', alignItems:'center', display:'flex',flexDirection:'column', top:'30px',fontSize:'31px'}}>
            <a href='#/titties' onClick={setPiss} style={{color:'#009451'}}>
                <h3>Piss</h3>
            </a>
            <a href='#/titties' onClick={setKhalifa} style={{color:'#F38'}}>
                <h4 style={{paddingTop:'60px'}}>khalifa</h4>
            </a>
            <a href='#/titties' onClick={setKhalifaArmy} style={{color:'#F38'}}>
                <h4 style={{paddingTop:'60px'}}>khalifa Army</h4>
            </a>
        </div>
            <Cockmaster3 />
        </div>
    );
}


export {TitsNBalls};
