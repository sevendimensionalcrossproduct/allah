<script lang="tsx">
import { defineComponent } from 'vue';

export default defineComponent({
  name: 'Cock',
  setup() {
    const message = 'Hello from Cock!';
    const details = 'Less allows us to use nesting, variables, and mixins!';

    return () => (
      <div class="container">
        <h2>This is Cock</h2>
        <p>{message}</p>
        <p class="details">{details}</p>
      </div>
    );
  },
});
</script>

<style lang="less" scoped>
@primary-color: green;
@text-size: 1.1rem;

.container {
  padding: 20px;
  background-color: #f0f0f0;

  h2 {
    color: @primary-color;
    font-size: 2rem;
    margin-bottom: 10px;
  }

  .details {
    font-size: @text-size;
    margin-top: 5px;
    color: lighten(@primary-color, 20%);

    &:hover {
      color: darken(@primary-color, 10%);
    }
  }
}
</style>
