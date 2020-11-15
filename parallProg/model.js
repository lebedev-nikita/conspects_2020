const { range } = require('range');

const N = 5;
const kRight = 1 / 2;

class Node {
  constructor(size, kRight) {
    this.size = size;
    this.kRight = kRight;
    this.right = size * kRight;
    this.bottom = size * (1 - kRight);
  }
}

class Matrix {
  constructor(n) {
    this.inner = new Array(n);
    for (let i = 0; i < n; i++) {
      this.inner[i] = new Array(n);
    }

    this.inner[0][0] = new Node(1, kRight);

    for (let i of range(1, n)) {
      this.inner[0][i] = new Node(this.inner[0][i - 1].right, kRight);
      this.inner[i][0] = new Node(this.inner[i - 1][0].bottom, kRight);
    }

    for (let i of range(1, n - 1)) {
      for (let j of range(1, n - 1)) {
        const nodeValue = this.inner[i][j - 1].right + this.inner[i - 1][j].bottom;
        this.inner[i][j] = new Node(nodeValue, kRight);
      }
    }

    for (let i of range(1, n - 1)) {
      const bottomValue = this.inner[n - 1][i - 1].size + this.inner[n - 2][i].bottom;
      const rightValue = this.inner[i - 1][n - 1].size + this.inner[i][n - 2].right;
      this.inner[n - 1][i] = new Node(bottomValue, kRight);
      this.inner[i][n - 1] = new Node(rightValue, kRight);
    }
    const lastValue = this.inner[n - 1][n - 2].size + this.inner[n - 2][n - 1].size;
    this.inner[n - 1][n - 1] = new Node(lastValue, kRight);
  }

  map(fun) {
    return this.inner.map((row) => row.map(fun))
  }
  table(key) {
    return this.inner.map((row) => row.map((elem) => elem[key]))
  }
}

const m = new Matrix(5);
console.table(m.table('size'))
