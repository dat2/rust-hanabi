const path = require('path');
require('dotenv-safe').load({ sample: path.resolve(__dirname, '../.env.default'), path: path.resolve(__dirname, '../.env') });

const gulp = require('gulp');
const debug = require('gulp-debug');
const rename = require('gulp-rename');
const template = require('gulp-template');
const shell = require('gulp-shell');
const plumber = require('gulp-plumber');
const del = require('del');

const paths = {
  src: path.resolve(__dirname, 'src'),
  static: path.resolve(__dirname, 'static'),
  dist: path.resolve(__dirname, 'build')
};

gulp.task('template', () => {
  gulp.src(paths.static + '/**/*.tpl')
    .pipe(template(process.env))
    .pipe(rename({ extname: '' }))
    .pipe(gulp.dest(paths.dist));
});

gulp.task('copy:static', () => {
  gulp.src([paths.static + '/**/*', '!' + paths.static + '/**/*.tpl'])
    .pipe(plumber())
    .pipe(debug())
    .pipe(gulp.dest(paths.dist));
});

gulp.task('clean', () => {
  return del([
    paths.dist + '/**/*'
  ]);
});

gulp.task('elm', shell.task([
  'elm make --output=build/main.js src/Main.elm'
]));

gulp.task('watch', () => {
  gulp.watch(paths.static + '/**/*.tpl', ['template']);
  gulp.watch([paths.static + '/**/*', '!' + paths.static + '/**/*.tpl'], ['copy:static']);
});

gulp.task('build', ['clean', 'elm', 'copy:static', 'template']);

gulp.task('default', ['build', 'watch']);
