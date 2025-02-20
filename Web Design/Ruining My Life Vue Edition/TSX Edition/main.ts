import { createApp } from 'vue';
import { createRouter, createWebHashHistory } from 'vue-router';
import { createPinia } from 'pinia';
import App from './App.vue';
import LeTsx from './routetsx.vue';
import Cock from './routefirst.vue';
import About from './routesecond.vue';
import Home from './routehome.vue';
import ElTailwindo from './ElTailwindo.vue';
const routes = [
    {path: '/', component: Home},
    {path: '/LeTsx', component: LeTsx},
    {path: '/Cock', component: Cock},
    {path: '/About', component: About},
    {path: '/ElTailwindo', component: ElTailwindo}
];

const router = createRouter({
    history: createWebHashHistory(),
    routes,
});

createApp(App)
    .use(router)
    .use(createPinia())
    .mount('#app')
