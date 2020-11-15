const { range } = require('range');

const N = 5;
const kRight = 1 / 2;

class Node {
  constructor(i, j, n, matrix) {
    if (0 == i && 0 == j) {
      this.size = 1;
    } else if (i == 0) {
      this.size = matrix[0][j - 1].right;
    } else if (j == 0) {
      this.size = matrix[i - 1][0].bottom;
    } else if (i == n - 1 && j == n - 1) {
      this.size = matrix[i - 1][j].size + matrix[i][j - 1].size;
    } else if (i == n - 1) {
      this.size = matrix[i - 1][j].bottom + matrix[i][j - 1].size;
    } else if (j == n - 1) {
      this.size = matrix[i - 1][j].size + matrix[i][j - 1].right;
    } else {
      this.size = matrix[i - 1][j].bottom + matrix[i][j - 1].right;
    }

    this.kRight = kRight;
    this.right = this.size * this.kRight;
    this.bottom = this.size * (1 - this.kRight);
  }
}

class Matrix {
  constructor(n) {
    this.inner = new Array(n);
    for (let i = 0; i < n; i++) {
      this.inner[i] = new Array(n);
    }

    for (let i = 0; i < n; i++) {
      for (let j = 0; j < n; j++) {
        this.inner[i][j] = new Node(i, j, n, this.inner)
      }
    }
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
