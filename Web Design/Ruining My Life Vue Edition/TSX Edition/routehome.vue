<script lang="tsx">
import { defineComponent } from 'vue';
import { RouterLink, RouterView } from 'vue-router';

export default defineComponent({
  name: 'MyComponent',
  setup() {
    const message = 'Hello from MyComponent!';

    return () => (
      <div>
        <h2>This is my component!</h2>
        <p>{message}</p>
        <nav>
          <RouterLink to="/Cock">Go to Cock</RouterLink> <br />
          <RouterLink to="/About">Go to About</RouterLink>
        </nav>
        <RouterView />
      </div>
    );
  },
});
</script>

<style lang="postcss" scoped>
h2 {
  color: blue;
}
</style>
