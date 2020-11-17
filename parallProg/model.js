const { range } = require('range');

const N = 5;
const _kRight = 1 / 2;

class Node {
  constructor(i, j, n, matrix) {
    // углы
    if (0 == i && 0 == j) {
      this.size = 1;
      this.kRight = _kRight;
    } else if (i == 0 && j == n - 1) {
      this.size = matrix[0][n - 2].right;
      this.kRight = 0;
    } else if (i == n - 1 && j == 0) {
      this.size = matrix[n - 2][0].bottom;
      this.kRight = 1;
    } else if (i == n - 1 && j == n - 1) {
      this.size = matrix[i - 1][j].size + matrix[i][j - 1].size;
      this.kRight = 1;
    }
    // границы
    else if (i == 0) {
      this.kRight = _kRight;
      this.size = matrix[0][j - 1].right;
    } else if (j == 0) {
      this.kRight = _kRight;
      this.size = matrix[i - 1][0].bottom;
    } else if (i == n - 1) {
      this.kRight = 1;
      this.size = matrix[i - 1][j].bottom + matrix[i][j - 1].size;
    } else if (j == n - 1) {
      this.kRight = 0;
      this.size = matrix[i - 1][j].size + matrix[i][j - 1].right;
    }
    // центральные
    else {
      this.kRight = _kRight;
      this.size = matrix[i - 1][j].bottom + matrix[i][j - 1].right;
    }

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

  table(keys) {
    return this.inner.map((row) => row.map((node) => {
      return keys.map((key) => node[key])
    }))
  }
}

const m = new Matrix(5);
console.log('bottom, right:');
console.table(m.table(['bottom', 'right']));
