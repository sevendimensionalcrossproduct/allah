import './App.scss'
import * as client from 'http://localhost:3001/client';

function App() {

  return (
    <>
      <div className='card'>
        <h1 className='hd'> Welcome Users </h1>

        <div className='butts'>
          <button className='butt' onClick={client.promptAndCreateUser}>CREATE USER</button>
          <button className='butt' onClick={client.promptAndUpdateUsername}>UPDATE USERNAME</button>
          <button className='butt' onClick={client.promptAndDeleteUsername}>DELETE USERNAME</button>
          <button className='butt' onClick={client.wipeUsers}>WIPE</button>
        </div>
        <div className='users'>
          <div id="output">
        </div>
        </div>              
      </div>
    </>
  )
}

export default App
