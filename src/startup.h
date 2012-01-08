/* define all scheme constants */
#define fx_mask              0x03
#define fx_tag               0x00
#define fx_shift                2
#define bool_f               0x2F
#define bool_t               0x6F
#define bool_bit                6
#define list_nil             0x3F
#define eof_obj              0x7F
#define char_mask            0x3F
#define char_tag             0x0F
#define char_shift              8
#define obj_mask             0x07
#define obj_shift               3
#define pair_tag             0x01
#define pair_size               8
#define pair_car                0
#define pair_cdr                4
#define vector_tag           0x05
#define string_tag           0x06
#define closure_tag          0x02
#define symbol_tag           0x03
#define closure_end          0x07
#define return_addr          0x17

/* all scheme values are of type ptrs */
typedef unsigned int ptr;

typedef struct {
  void* eax; /* 0   scratch  */
  void* ebx; /* 4   preserve */
  void* ecx; /* 8   scratch  */
  void* edx; /* 12  scratch  */
  void* esi; /* 16  preserve */
  void* edi; /* 20  preserve */
  void* ebp; /* 24  preserve */
  void* esp; /* 28  preserve */
} context;

typedef struct {
  ptr car;
  ptr cdr;
} cell;

typedef struct {
  ptr length;
  ptr buf[1];
} vector;

typedef struct {
  ptr length;
  char buf[1];
} string;

typedef struct {
  char* heap_cur;
  char* global_cur;
  char* heap_base;
  char* heap_top;
  char* heap_base_alt;
  char* heap_top_alt;
  char* global_base;
  char* stack_base;
} memory;
