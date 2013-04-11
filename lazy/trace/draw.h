/* simple drawing functions */

#ifndef DRAW_H
#define DRAW_H

#include "image.h"
#include "point.h"

/* draw a point */ 
template <class T>
inline void draw_point(image<T> *im, const T &val, const point<int> &p) {
  if ((p.x >= 0) && (p.x < im->width()) &&
      (p.y >= 0) && (p.y < im->height()))
    imRef(im, p.x, p.y) = val;
}

/* draw a square */
template <class T>
void draw_square(image<T> *im, const T &val, const point<int> &p, int size) {
  point<int> l;

  for (l.y = p.y-size; l.y <= p.y+size; l.y++)
    for (l.x = p.x-size; l.x <= p.x+size; l.x++)
      draw_point(im, val, l);
}

/* draw a cross */
template <class T>
void draw_cross(image<T> *im, const T &val, const point<int> &p, int size) {
  for (int yp = p.y-size; yp <= p.y+size; yp++)
    draw_point(im, val, point<int>(p.x, yp));
  for (int xp = p.x-size; xp <= p.x+size; xp++)
    draw_point(im, val, point<int>(xp, p.y));
}

/* draw a line segment */
template <class T>
void draw_line(image<T> *im, const T &val, point<int> p1, point<int> p2) {
  point<int> v = p2-p1;
  point<int> a = abs(v);
  point<int> s = sign(v);
  a.x << 1;
  a.y << 1;

  if (a.x > a.y) {
    /* x dominant */
    int d = a.y - (a.x >> 1);
    for (;;) {
      draw_point(im, val, p1);
      if (p1.x == p2.x) return;
      if (d > 0) {
	p1.y += s.y;
	d -= a.x;
      }
      p1.x += s.x;
      d += a.y;
    }
  } else {
    /* y dominant */
    int d = a.x - (a.y >> 1);
    for (;;) {
      draw_point(im, val, p1);
      if (p1.y == p2.y) return;
      if (d > 0) {
	p1.x += s.x;
	d -= a.y;
      }
      p1.y += s.y;
      d += a.x;
    }
  }
}

/* draw a rectangle */
template <class T>
void draw_square(image<T> *im, const T &val, 
		 const point<int> &a, const point<int> &b) {
  for (int x = a.x; x < b.x; x++) {
    draw_point(im, val, point<int>(x, a.y));
    draw_point(im, val, point<int>(x, b.y));
  }
  for (int y = a.y; y < b.y; y++) {
    draw_point(im, val, point<int>(a.x, y));
    draw_point(im, val, point<int>(b.x, y));
  }
}

#endif
