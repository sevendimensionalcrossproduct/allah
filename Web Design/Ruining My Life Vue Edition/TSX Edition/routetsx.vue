<script lang="tsx">
import { defineComponent} from 'vue';
import { defineStore, storeToRefs, createPinia } from 'pinia';

const useCounterStore = defineStore('counter', {
    state: () => ({
        count: 0,
    }),
    actions: {
        increment() {
            this.count++;
        },
        decrement() {
            this.count--;
        },
    },
});

export default defineComponent({
  name: 'MyTsxComponent',
  setup() {
    const counter = useCounterStore(createPinia());
    const {count} = storeToRefs(counter);

    return () => (
      <div>
        <h1>Welcome to My TSX Component</h1>
        <button onClick={counter.increment}>The count</button> |
        <button onClick={counter.decrement}>uncount</button> |
        <p>Counter count: {count.value} </p>
        <ul class="color-list">
            <li>also</li> 
            <li>now</li> 
            <li>scss</li> 
            <li>edition</li> 
            <li>lmao</li> 
        </ul>
        {count.value > 10 ? <p>🥵</p> : <p>click le button</p>}
      </div>
    );
  },
});
</script>

<style lang="scss" scoped>
@use "sass:list";
$colors: #ff9999, #ffcc99, #ffff99, #ccff99, #99ccff;

.color-list {
  list-style-type: none;
  font-size: 24px;
  padding: 0;

  li {
    @for $i from 1 through list.length($colors) {
      &:nth-child(#{$i}) {
        color: list.nth($colors, $i);
      }
    }
  }
}

h1 {
  color: salmon;
}
button {
  font-size: 16px;
}
</style>

