<template>
  <div>
    <h2>This is cock</h2>
    <p>{{ message }}</p>
  </div>
</template>

<script lang="ts">
export default {
    name: 'Cock',
  data() {
    return {
      message: 'Hello from Cock!',
    };
  },
};
</script>



<style scoped>
h2 {
    color: green;
}
</style>
