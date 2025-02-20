<template>
  <div>
    <h2>This is about</h2>
    <p>{{ message }}</p>
      <router-link to='/'>sdfsdgsdfg</router-link>
  </div>
</template>

<script lang="ts">
export default {
    name: 'About',
  data() {
    return {
      message: 'Hello from about!',
    };
  },
};
</script>


<style scoped>
h2 {
    color: red;
}
</style>
