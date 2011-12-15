/* define all scheme constants */
#define fx_mask              0x03
#define fx_tag               0x00
#define fx_shift                2
#define bool_f               0x2F
#define bool_t               0x6F
#define bool_bit                6
#define list_nil             0x3F
#define char_mask            0x3F
#define char_tag             0x0F
#define char_shift              8


/* all scheme values are of type ptrs */
typedef unsigned int ptr;
