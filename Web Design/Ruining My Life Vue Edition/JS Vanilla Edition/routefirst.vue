<template>
  <div>
    <h2>This is cock</h2>
    <p>{{ message }}</p>
  </div>
</template>

<script>
const Cock = {
    name: 'Cock',
  data() {
    return {
      message: 'Hello from Cock!',
    };
  },
};
window.Cock = Cock;
export default window.Cock;
</script>



<style scoped>
h2 {
    color: green;
}
</style>
