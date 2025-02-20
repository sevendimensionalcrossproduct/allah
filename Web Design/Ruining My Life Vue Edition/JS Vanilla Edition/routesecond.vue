<template>
  <div>
    <h2>This is about</h2>
    <p>{{ message }}</p>
      <router-link to='/'>sdfsdgsdfg</router-link>
  </div>
</template>

<script>
const About = {
    name: 'About',
  data() {
    return {
      message: 'Hello from about!',
    };
  },
};
window.About = About;
export default About;
</script>


<style scoped>
h2 {
    color: red;
}
</style>
