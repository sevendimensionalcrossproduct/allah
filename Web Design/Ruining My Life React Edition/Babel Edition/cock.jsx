const { useState } = React;

function LeButtion({ count, setCount }){
    return <button onClick={() => setCount(count+1)}>penor</button>
}

function penis(){
    const [count, setCount] = React.useState(0);
    return (
        <> {/* fragment to not have a shitload of pointless wrapping divs*/}
            
            <p>ur mom</p>
            <LeButtion count={count} setCount={setCount}/>
            <p>le cound is {count}</p>
        </>
    );
}
