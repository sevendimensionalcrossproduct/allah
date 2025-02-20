//we need this garbage for ts to import vue files
declare module '*.vue' {
    import { DefineComponent } from 'vue';
    const component: DefineComponent<{}, {}, any>;
    export default component;
}

declare namespace JSX {
  interface IntrinsicElements {
    [elem: string]: any; // Add a generic type for JSX elements
  }
}

