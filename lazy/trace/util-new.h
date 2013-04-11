#ifndef UTIL_H
#define UTIL_H

/* basic stuff */

rgb red = {255,0,0};
rgb green = {0,255,0};
rgb blue = {0,0,255};

void berror(char *s) {
  fprintf(stderr,"%s\n",s);
  exit(-1);
}

float min(float a, float b) {
  return (a <= b ? a : b);
}

float max(float a, float b) {
  return (a >= b ? a : b);
}

float min(float a, float b, float c) {
  return min(a, min(b, c));
}

float max(float a, float b, float c) {
  return max(a, max(b, c));
}

/* the packing macros */

#define pack2(a,alength,b,blength) (((a) << (blength)) + (b))

#define unpack2(a,alength,b,blength,z)			\
  int a = (z >> (blength) & ((1 << alength)-1));	\
  int b = (z & ((1 << blength)-1));

#define pack3(a,alength,b,blength,c,clength) (((a) << (blength+clength)) + ((b) << clength) + (c))

#define unpack3(a,alength,b,blength,c,clength,z) \
  int a = ((z >> (blength + clength)) & ((1 << alength)-1)); \
  int b = ((z >> clength) & ((1 << blength)-1)); \
  int c = (z & ((1 << clength)-1));

#define pack4(a,alength,b,blength,c,clength,d,dlength) (((a) << (blength+clength+dlength)) + ((b) << clength+dlength) + ((c) << dlength) + (d))

#define unpack4(a,alength,b,blength,c,clength,d,dlength,z) \
  int a = (z >> (blength + clength + dlength)) & ((1 << alength)-1); \
  int b = (z >> (clength + dlength)) & ((1 << blength)-1); \
  int c = (z >> dlength) & ((1 << clength)-1); \
  int d = z & ((1 << dlength)-1);

#define pack5(a,alength,b,blength,c,clength,d,dlength,e,elength) (((a) << (blength+clength+dlength+elength))  + ((b) << clength+dlength+elength) + ((c) << (dlength+elength)) + ((d) << elength) + (e))

#define unpack5(a,alength,b,blength,c,clength,d,dlength,e,elength,z) \
  int a = (z >> (blength + clength + dlength+elength)) & ((1 << alength)-1); \
  int b = (z >> (clength + dlength+elength)) & ((1 << blength)-1); \
  int c = (z >> (dlength + elength)) & ((1 << clength)-1); \
  int d = (z >>  elength) & ((1 << dlength)-1); \
  int e = z & ((1 << elength)-1);

/* the hash table:
 * maps (length,seg) to (weight,pre_seg)
 * length should never be 0. */

/* hashtable entry */
typedef struct entrystr {
  int length, seg;
  float weight;
  int pre_seg;
} *entry;

/* hashtable struct */
typedef struct htablestr {
  int size;              // size of table
  entry start;           // entries
  int entry_count;       // number entries initialized by intern
  int intern_count;      // number of hashtable accesses 
  int intern_collision;  // number of collisions  
} *htable;

/* re-initialize hashtable */
void clear_htable(htable t){
  /*
  for (int key = 0; key < t->size; key++)
    t->start[key].length = 0;
  */
  memset(t->start, 0, t->size * sizeof(entrystr));
  t->entry_count = 0;
  t->intern_count = 0;
  t->intern_collision = 0;
}

/* allocate new hashtable */
htable alloc_htable(int size) {
  htable t = new htablestr;
  t->size = size;
  t->start = new entrystr[size];
  clear_htable(t);
  return t;
}

/* deallocate hashtable */
void free_htable(htable t) {
  delete [] t->start;
  delete t;
}

/* find spot for entry but don't reserve if empty */
inline entry preintern(htable t, int length, int seg){
  t->intern_count++;
  
  unsigned int h = 
    729*length + 729*(length >> 12) + 729*(length >> 24) + 
    125*seg + 125*(seg >> 12) + 125*(seg >> 24); 
  
  unsigned int key = h % t->size;
  while ((t->start[key].length != 0) && 
	 (t->start[key].length != length || t->start[key].seg != seg)) {
    t->intern_collision++;
    key = (key + 1) % t->size;
  }

  // did the hashtable degenerate?
  if (t->intern_collision >= 4*t->intern_count)
    berror("too many collisions");

  return t->start + key;
}

/* find spot for entry and reserve/initialize if necessary */
entry intern(htable t, int length, int seg, float weight, int pre_seg){
  // is the hashtable full?
  if (2*t->entry_count >= t->size)
    berror("hashtable full");

  entry e = preintern(t, length, seg);
  if (e->length == 0){
    t->entry_count++;
    e->length = length;
    e->seg = seg;
    e->weight = weight;
    e->pre_seg = pre_seg;
  }
  return e;
}

/* the queue */

#define HEAP_UP(pos) ((pos - 1) >> 1)
#define HEAP_DOWN(pos) ((pos << 1) + 1)

typedef struct itemstr {
  int id;
  void *data;
  int pri;
} *item;

typedef struct queuestr {
  item heap;
  int size;
  int num;
  int next_id;
} *queue;

bool less(itemstr &a, itemstr &b) {
  if ((a.pri < b.pri) || ((a.pri == b.pri) && (a.id < b.id)))
    return true;
  else
    return false;
}

void swap(itemstr &a, itemstr &b) {
  itemstr tmp = a;
  a = b;
  b = tmp;
}

/* allocate new queue */
queue alloc_queue(int size) {
  queue q = new queuestr;
  q->heap = new itemstr[size];
  q->size = size;
  q->num = 0;
  q->next_id = 0;
  return q;
}

/* deallocate queue */
void free_queue(queue q) {
  delete [] q->heap;
  delete q;
}

void bubble_up(queue q, int pos) {
  if (pos == 0)
    return;
  int up = HEAP_UP(pos);
  if (less(q->heap[pos], q->heap[up])) {
    swap(q->heap[up], q->heap[pos]);
    bubble_up(q, up);
  }
}

void bubble_down(queue q, int pos) {
  int down = HEAP_DOWN(pos);
  if (down >= q->num)
    return;
  int tmp = down+1;
  if ((tmp < q->num) && less(q->heap[tmp], q->heap[down]))
    down = tmp;

  if (!less(q->heap[pos], q->heap[down])) {
    swap(q->heap[down], q->heap[pos]);
    bubble_down(q, down);
  }
}

/* push item into queue */
void push(FILE *info, queue q, int pri, void *v){
  if (q->num >= q->size)
    berror("full queue");

  fprintf(info, "Insert: %d at priority %d\n", q->next_id, pri);

  q->heap[q->num].id = q->next_id;
  q->heap[q->num].data = v;
  q->heap[q->num].pri = pri;
  bubble_up(q, q->num);
  q->num++;
  q->next_id++;
}

/* pop lowest priority item */
void *pop(FILE *info, queue q){
  if (q->num == 0) 
    berror("empty queue");

  fprintf(info, "RemoveMin: %d\n", q->heap[0].id);

  q->num--;
  swap(q->heap[0], q->heap[q->num]);
  bubble_down(q, 0);
  return q->heap[q->num].data;
}

#endif
