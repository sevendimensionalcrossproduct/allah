<script lang="tsx">
import { defineComponent } from 'vue';
import { RouterLink } from 'vue-router';

export default defineComponent({
  name: 'About',
  setup() {
    const message = 'Hello from about!';
    const additionalInfo = 'Stylus supports variables, nesting, and mixins!';

    return () => (
      <div class="about-container">
        <h2>This is About</h2>
        <p>{message}</p>
        <p class="additional-info">{additionalInfo}</p>
        <RouterLink to="/">Back to Home</RouterLink>
      </div>
    );
  },
});
</script>

<style lang="stylus" scoped>
primary-color = #3498db
secondary-color = lighten(primary-color, 20%)
text-padding = 10px

.about-container
  padding 20px
  background-color #f5f5f5

  h2
    color primary-color
    font-size 2rem
    margin-bottom 5px

  .additional-info
    color secondary-color
    padding text-padding
    font-style italic
    border 1px solid primary-color
    border-radius 5px
    &:hover
      color darken(primary-color, 10%)
      border-color darken(primary-color, 10%)

  a
    color primary-color
    text-decoration none
    &:hover
      color darken(primary-color, 15%)
</style>
