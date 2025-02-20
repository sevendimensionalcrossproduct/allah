<script lang="tsx">
import { defineComponent } from 'vue';
import { defineStore, storeToRefs, createPinia } from 'pinia';

const useClickStore = defineStore('shark', { // overengineered as shit lmao but wanna use pinia for no reason
    state: () => ({
        rendered: false,
    }),
    actions: {
        render() {
            this.rendered = true;
        },
        unrender() {
            this.rendered = false;
        }
    }
});

export default defineComponent({
    name: 'ElTailwindo',
    setup() {
        const shark = useClickStore(createPinia());
        const { rendered } = storeToRefs(shark); // Changed `isRendered` to `rendered`

        return () => ( // Added a render function here
            <div class="max-w-sm mx-auto bg-white rounded-lg shadow-lg overflow-hidden">
                <div class="p-6">
                    <h2 class="text-2xl font-bold text-gray-800">This is Le Tailwind!</h2>
                    <p class="mt-2 text-gray-600">
                        click on le button for magic
                    </p>
                    <button
                        onClick={shark.render}
                        class="mt-4 w-full bg-blue-500 text-white font-semibold py-2 rounded hover:bg-blue-600 transition duration-200"
                    >
                        le button
                    </button>
                    {rendered.value == true ? (
                    <button
                        onClick={shark.unrender}
                        class="mt-4 w-full bg-red-500 text-white font-semibold py-2 rounded hover:bg-blue-600 transition duration-200"
                    >
                        kill shark (abuse)
                    </button>
                    ) : null
                    }
                    {rendered.value === true ? ( // Changed comparison to strict equality
                        <div class="font-mono whitespace-pre text-center bg-gradient-to-tr 
                        from-red-300 via-yellow-300 to-blue-300 text-transparent bg-[200%] bg-clip-text">
                            <p> _________         .    .</p>
                            <p>(..       \_    ,  |\  /|</p>
                            <p> \       O  \  /|  \ \/ /</p>
                            <p>  \______    \/ |   \  / </p>
                            <p>     vvvv\    \ |   /  | </p>
                            <p>     \^^^^  ==   \_/   | </p>
                            <p>      `\_   ===    \.  | </p>
                            <p>      / /\_   \ /      | </p>
                            <p>      |/   \_  \|      / </p>
                            <p>             \________/  </p>
                        </div>
                    ) : null}
                </div>
            </div>
        );
    }
});
</script>

<style scoped>
@import 'tailwindcss/base';
@import 'tailwindcss/components';
@import 'tailwindcss/utilities';
</style>
