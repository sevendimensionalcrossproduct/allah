import { createApp, } from 'vue';
import { createRouter, createWebHashHistory } from 'vue-router';
import App from './App.vue';
import MyComponent from './routehome.vue';
import Cock from './routefirst.vue';
import About from './routesecond.vue';

const routes = [
    {path: '/', component: MyComponent},
    {path: '/Cock', component: Cock},
    {path: '/About', component: About}
];

const router = createRouter({
    history: createWebHashHistory(),
    routes,
});

createApp(App).use(router).mount('#app')
