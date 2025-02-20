import * as crud from 'http://localhost:3001/crud';
window.crud = crud;
export const showUsers = async () => {
    try {
        const response = await fetch(crud.usersUrl);
        const data = await response.json();
        const outputElement = document.getElementById('output');
        outputElement
            ? (outputElement.innerText = JSON.stringify(data, null, 2))
            : console.error('Output element not found.');
    }
    catch (error) {
        console.error('bad: ', error);
    }
};
export const promptAndCreateUser = async () => {
    crud.specifyUserData('Enter new username', 'Invalid username', async (newUsername) => {
        const newUser = { userId: -1, userName: newUsername }; //ID calculated by API
        try {
            await crud.createFetch(newUser);
            alert(`User ${newUsername} created successfully`);
            await showUsers();
        }
        catch (error) {
            console.error('bad: ', error);
        }
    });
};
export const promptAndGetUsername = async () => {
    crud.specifyUserData('Enter ID to get:', 'No ID provided', async (userId) => {
        try {
            await crud.findId(userId, (user) => alert(`Username for user ID ${userId}: ${user.userName}`));
        }
        catch (error) {
            console.error('bad: ', error);
        }
    });
};
export const promptAndUpdateUsername = async () => {
    crud.specifyUserData('Enter ID to update:', 'No ID provided', async (userId) => {
        try {
            await crud.findId(userId, async () => {
                crud.specifyUserData('Enter updated username', 'Invalid username', async (updatedUser) => {
                    const response = await crud.updateFetch(userId, updatedUser);
                    if (response.ok) {
                        const updatedUserData = await response.json();
                        alert(`Username updated successfully: ${updatedUserData.userName}`);
                        await showUsers();
                    }
                    else {
                        throw new Error(`bad ${response.status}`);
                    }
                });
            });
        }
        catch (error) {
            console.error('bad: ', error);
        }
    });
};
export const promptAndDeleteUsername = async () => {
    crud.specifyUserData('Enter ID to delete:', 'No ID provided', async (userId) => {
        try {
            await crud.findId(userId, async (user) => {
                await crud.deleteFetch(userId);
                await showUsers();
                alert(`Username ${user.userName} was successfully deleted`);
            });
        }
        catch (error) {
            console.error('bad: ', error);
        }
    });
};
export const wipeUsers = async () => {
    try {
        await crud.deleteFetch();
        alert('Userbase wiped successfully');
        await showUsers();
    }
    catch (error) {
        console.error(`bad: `, error);
    }
};
export const hideUsers = () => {
    const outputElement = document.getElementById('output');
    outputElement.innerText = '';
};
window.showUsers = showUsers;
window.createUser = promptAndCreateUser;
window.getUsername = promptAndGetUsername;
window.updateUsername = promptAndUpdateUsername;
window.deleteUsername = promptAndDeleteUsername;
window.wipe = wipeUsers;
window.hideUsers = hideUsers;
