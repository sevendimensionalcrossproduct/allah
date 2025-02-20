<template>
  <div>
    <h2>This is my component!</h2>
    <p>{{ message }}</p>
    <nav>
      <router-link to="/Cock">Go to Cock </router-link> <br>
      <router-link to="/About">Go to About</router-link>
    </nav>
    <router-view></router-view>

  </div>
</template>

<script>
const MyComponent = {
    name: 'MyComponent',

  data() {
    return {
      message: 'Hello from MyComponent!',
    };
  },
}
export default MyComponent

</script>

<style scoped>
h2 {
    color: blue;
}
</style>
