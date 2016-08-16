const path = require('path');
require('dotenv-safe').load({ sample: path.resolve(__dirname, '../.env.default'), path: path.resolve(__dirname, '../.env') });

const gulp = require('gulp');
const template = require('gulp-template');

const paths = {
  src: path.resolve(__dirname, 'src'),
  dist: path.resolve(__dirname, 'build')
};
paths.index = paths.src + '/index.html';

gulp.task('template', () => {
  gulp.src(paths.index)
    .pipe(template(process.env))
    .pipe(gulp.dest(paths.dist));
});

gulp.task('watch', () => {
  gulp.watch(paths.index, ['template']);
});

gulp.task('default', ['template', 'watch']);
