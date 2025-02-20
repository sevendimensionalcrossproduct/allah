import React from 'react';
import { Cockmaster3} from './Cockmaster3'


function AssNBalls() {
    const [isSpinning, setIsSpinning] = React.useState(false);
    const TheDonkThatSpins = () => {
        setIsSpinning(!isSpinning);
    };


    return(
        <div>
            <h2 className="penis" style= {{position: 'absolute', left: '305px', color:'#94003b'}}>spindonk</h2>
            <img src='https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/359fe050-3abb-4764-b470-33c7883ce044/de8zzdt-cec9e0c0-1ef2-4574-8cb8-0e47ef45bae1.png?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7InBhdGgiOiJcL2ZcLzM1OWZlMDUwLTNhYmItNDc2NC1iNDcwLTMzYzc4ODNjZTA0NFwvZGU4enpkdC1jZWM5ZTBjMC0xZWYyLTQ1NzQtOGNiOC0wZTQ3ZWY0NWJhZTEucG5nIn1dXSwiYXVkIjpbInVybjpzZXJ2aWNlOmZpbGUuZG93bmxvYWQiXX0.OwBltZJfOCeEabVLe1QmYNX_ID1qnee-tmO4uWJ7Yrw' className={isSpinning? 'spindonk' : 'donk'} onClick = {TheDonkThatSpins}/>
            <Cockmaster3/>
        </div>
    );
}

export { AssNBalls } 
