#include <list>
#include <algorithm>
#include <cstdio>
#include <cassert>
#include "image.h"
#include "pnmfile.h"
#include "imconv.h"
#include "point.h"
#include "draw.h"
#include "util-new.h"

/*
 * shortcuts:
 * one base case per pixel (instead of subpixel) per orientation.
 */

using namespace std;

#define SUBLEVEL 1       // subpixel level
#define MARK_RADIUS 2    // area that is covered by a curve point 
#define SCALE 100        // float to int scale
#define WEIGHT 400       // maximum initial weight
#define TURN   00        // cost of turning
#define FILTERL 3
#define FILTERW 2

#define QSIZE (1 << 22)
#define HSIZE (1 << 22)

#define INF_FLOAT 1E10

// curve point
typedef point<int> location;       

image<uchar> *input;     // input image
image<uchar> *upsampled; // upsampled input
image<int> *cover;      // 1 if pixel needs to be covered, 0 otherwise
image<float> **data;    // cost of each segment
int A;                  // constant in density calculation
int maxlength;          // maximum curve length considered
int num;                // number of curves to get
queue q;                // the queue
htable h;               // the hashtable
float best_val;             // best density so far
int best_seg, best_length;  // best curve so far
int thetadim;               // number of orientations
int *thetadx, *thetady;     // offsets of different orientations

FILE *info;

/*
  seg packs endpoint (x,y,theta,dir) where (x,y) are subpixel locations.
  dir is 0 or 1 indicating if curve turns left or right
  cover is indexed by pixels.
  data cost is indexed by subpixels and theta
  hashtable maps (length, seg) -> (weight, pre_seg).
*/

#define pack_seg(x, y, t, d) pack4(x, 12, y, 12, t, 7, d, 1)
#define unpack_seg(x, y, t, d, seg) unpack4(x, 12, y, 12, t, 7, d, 1, seg)

inline void expand(int pre_length, int pre_seg, float pre_weight, 
		   int x, int y, int t, int d) { 
  if (check_bound(x, 0, upsampled->width()-1) || 
      check_bound(y, 0, upsampled->height()-1))
    return;
  int length = pre_length + imRef(cover, x >> SUBLEVEL, y >> SUBLEVEL);
  int seg = pack_seg(x, y, t, d);
  float weight = pre_weight + imRef(data[t], x, y);
  entry e = preintern(h, length, seg);
  if ((e->length == 0) || (e->weight > weight)) {
    e->length = length;
    e->seg = seg;
    e->weight = weight;
    e->pre_seg = pre_seg;
    int pri = (int)(weight / length);
    push(info, q, pri, (void *)e);
  }
}

template <class T>
image<T> *upsample(image<T> *im, int level) {
  int width = im->width() << level;
  int height = im->height() << level;
  image<T> *out = new image<T>(width, height);
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      imRef(out, x, y) = imRef(im, x >> level, y >> level);
    }
  }
  return out;
}

void init_theta(){
  int k = 1 << SUBLEVEL;
  thetadim = 8*k;
  printf("thetadim = %d\n", thetadim);
  thetadx = new int[thetadim];
  thetady = new int[thetadim];
  int theta = 0;
  int dx = k;
  int dy = 0;
  for(;dy > -k;dy--){
	thetadx[theta]=dx;
	thetady[theta]=dy;
	theta++;}
  for(;dx > -k;dx--){
	thetadx[theta]=dx;
	thetady[theta]=dy;
	theta++;}
  for(;dy < k;dy++){
	thetadx[theta]=dx;
	thetady[theta]=dy;
	theta++;}
  for(;dx < k;dx++){
	thetadx[theta]=dx;
	thetady[theta]=dy;
	theta++;}
  for(;dy > 0;dy--){
	thetadx[theta]=dx;
	thetady[theta]=dy;
	theta++;}
  assert(theta == thetadim);
}

image<int> *grad(image<uchar> *im, int dx, int dy, int l, int w) {
  int width = im->width();
  int height = im->height();
  image<int> *f = new image<int>(width, height);

  // perpendicular displacement 
  int tx = dy;
  int ty = -dx;

  int start_x = abs(dy*w)+abs(dx*l);
  int end_x = width-start_x;
  int start_y = abs(dx*w)+abs(dy*l);
  int end_y = height-start_y;

  for (int y = start_y; y < end_y; y++) {
    for (int x = start_x; x < end_x; x++) {
      int val = 0;
      for (int i = 1; i <= w; i++) {
	for (int j = -l; j <= l; j++) {
	  val += 
	    (imRef(im, x + dx*j + tx*i, y + dy*j + ty*i) >> (i-1)) -
	    (imRef(im, x + dx*j - tx*i, y + dy*j - ty*i) >> (i-1));
	}
      }
      imRef(f, x, y) = val;
    }
  }

  return f;
}

image<float> **comp_data() {
  image<int> *f[thetadim];
  for (int t = 0; t < thetadim; t++)
    f[t] = grad(upsampled, thetadx[t], thetady[t], FILTERL, FILTERW);

  int width = upsampled->width();
  int height = upsampled->height();

  int maxgrad = 0;
  for (int i = 0; i < FILTERW; i++)
    maxgrad += (FILTERL*2+1) * (255 >> i);
  float maxcost = log(1.0 + maxgrad);

  image<float> **data = new image<float> *[thetadim];
  for (int t = 0; t < thetadim; t++) {
    data[t] = new image<float>(width, height);
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
	float v1 = max(imRef(f[t], x, y), 0);
	float v2 = abs(imRef(f[(t + thetadim/4) % thetadim], x, y));
	float val = max(v1-v2, 0);
	imRef(data[t], x, y) = (maxcost - log(1.0 + val)) * SCALE;
      }
    }
  }

  for (int t = 0; t < thetadim; t++)
    delete f[t];

  return data;
}

void init() {
  // assumptions
  assert(sizeof(int) == 4);
  assert(sizeof(float) == 4);

  upsampled = upsample(input, SUBLEVEL);
  q = alloc_queue(QSIZE);
  h = alloc_htable(HSIZE);
  init_theta();
  data = comp_data();
  cover = new image<int>(input->width(), input->height(), false);
  cover->init(1);
  best_val = INF_FLOAT;
  best_seg = 0;
  best_length = 0;
}

void base_case() {
  int width = upsampled->width();
  int height = upsampled->height();
  /*  
  for (int y = 0; y < height; y += 1) { 
    for (int x = 0; x < width; x += 1) {
  */
      
  for (int y = 0; y < height; y += (1 << SUBLEVEL)) {
    for (int x = 0; x < width; x += (1 << SUBLEVEL)) {

      for (int t = 0; t < thetadim; t++) {
	// only consider starting at non-covered pixels that are good
	if (imRef(cover, x >> SUBLEVEL, y >> SUBLEVEL) == 1 &&
	    imRef(data[t], x, y) < WEIGHT) {
	  expand(0, pack_seg(x, y, t, 0), 0, x, y, t, 0);
	  expand(0, pack_seg(x, y, t, 1), 0, x, y, t, 1);
	}
      }
    }
  }
}

void run() {
  while (q->num > 0) {
    entry e = (entry)pop(info, q);
    unpack_seg(x, y, t, d, e->seg);
    
    // check if this is the best curve so far
    float val = (e->weight+A)/e->length;
    if (val < best_val) {
      best_val = val;
      best_seg = e->seg;
      best_length = e->length;
    }
    
    // expand
    if (e->length < maxlength) {
      expand(e->length, e->seg, e->weight, x+thetadx[t], y+thetady[t], t, d);
      if (d == 0) 
	expand(e->length, e->seg, e->weight + TURN, x+thetadx[t], y+thetady[t], 
	       (t+1) % thetadim, 0);
      else
	expand(e->length, e->seg, e->weight + TURN, x+thetadx[t], y+thetady[t], 
	       (t+thetadim-1) % thetadim, 1);
    }
  
    if ((q->num > 0) && (best_val <= q->heap[0].pri)) {
      printf("stopping criteria!\n");
      break;
    }
  }

  printf("queued: %d\n", q->next_id);
  printf("best length: %d\n", best_length);
  printf("best value:  %f\n", best_val);
}

list<int> *trace(int seg, int length) {
  list<int> *curve = new list<int>;
  while (length > 0) {
    unpack_seg(x, y, t, d, seg);
    curve->push_back(seg);
    entry e = intern(h, length, seg, 0, 0);
    seg = e->pre_seg;
    length--;
  }
  return curve;
}

template <class T>
void draw(image<T> *im, T val, list<int> *curve) {
  list<int>::iterator ref = curve->begin();
  while (ref != curve->end()) {
    unpack_seg(x, y, t, d, *ref);
    imRef(im, x >> SUBLEVEL, y >> SUBLEVEL) = val;
    ref++;
  }
}

template <class T>
void draw_sub(image<T> *im, T val1, T val2, list<int> *curve) {
  list<int>::iterator ref = curve->begin();
  int xp = -1, yp = -1;
  while (ref != curve->end()) {
    unpack_seg(x, y, t, d, *ref);
    if ((xp != -1) && (yp != -1))
      draw_line(im, val1, location(x, y), location(xp, yp));
    xp = x;
    yp = y;
    ref++;
  }
  unpack_seg(x, y, t, d, *(curve->begin()));
  draw_square(im, val2, location(x, y), 1);
  draw_square(im, val2, location(xp, yp), 1);
}

void mark_seg(int seg) {
  unpack_seg(x, y, t, d, seg);
  draw_point(cover, 0, location(x >> SUBLEVEL, y >> SUBLEVEL));
  for (int dt = -thetadim/8; dt <= thetadim/8; dt++) {
    // perpendicular displacement 
    int tx = thetady[(t+dt+thetadim)%thetadim];
    int ty = -thetadx[(t+dt+thetadim)%thetadim];
    for (int i = 1; i <= MARK_RADIUS; i++) {
      draw_point(cover, 0, location((x + tx*i) >> SUBLEVEL, 
				    (y + ty*i) >> SUBLEVEL));
      draw_point(cover, 0, location((x - tx*i) >> SUBLEVEL, 
				    (y - ty*i) >> SUBLEVEL));
    }
  }
}

void mark(list<int> *curve) {
  list<int>::iterator ref = curve->begin();
  while (ref != curve->end()) {
    mark_seg(*ref);
    ref++;
  }
}

int main(int argc, char **argv) {
  if (argc != 4) {
    fprintf(stderr, "usage: %s input(pgm) A maxlength\n", argv[0]);
    exit(1);
  }

  input = loadPGM(argv[1]);
  A = (int)(atof(argv[2]) * SCALE);
  maxlength = atoi(argv[3]);

  info = fopen("data", "w");

  printf("init\n");
  init();

  image<rgb> *output = imageGRAYtoRGB(input);    
  image<rgb> *output_sub = upsample(output, SUBLEVEL);

  printf("base case\n");
  base_case();
  printf("queued: %d\n", q->next_id);
  printf("run\n");
  run();
  printf("trace\n");
  list<int> *curve = trace(best_seg, best_length);

  // draw
  draw(output, red, curve);
  savePPM(output, "out.ppm");
  draw_sub(output_sub, red, blue, curve);
  savePPM(output_sub, "out-sub.ppm");
  delete curve;

  fclose(info);

  return 0;
}

