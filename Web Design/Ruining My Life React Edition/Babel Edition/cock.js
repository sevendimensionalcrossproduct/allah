const {
  useState
} = React;
function LeButtion({
  count,
  setCount
}) {
  return /*#__PURE__*/React.createElement("button", {
    onClick: () => setCount(count + 1)
  }, "penor");
}
function penis() {
  const [count, setCount] = React.useState(0);
  return /*#__PURE__*/React.createElement(React.Fragment, null, " ", /*#__PURE__*/React.createElement("p", null, "ur mom"), /*#__PURE__*/React.createElement(LeButtion, {
    count: count,
    setCount: setCount
  }), /*#__PURE__*/React.createElement("p", null, "le cound is ", count));
}
