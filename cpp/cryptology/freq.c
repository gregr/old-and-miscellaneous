/*
	From adk0212@rit.edu Fri Sep 18 10:55 EDT 1998
	Subject: Frequency analysis program for Crypto
	         written by Adam D Kropelin

	compile with 

		cc -o freq freq.c

	usage
	 	freq inputfilename num_digrams num_trigrams

	where inputfilename: name of text file to compute frequencies on
       		num_digrams: number of digrams to provide results for
               num_trigrams: number of trigrams to provide results for

	An additional feature of this program is that it ignores all punctuation
	(including whitespace), which allows you to run it on formatted plaintext.
*/


#include <stdio.h>
#include <string.h>

int next_text_char( FILE *infile );

int main( int argc, char *argv[] ) {

  FILE *infile;
  int c1, c2, c3;
  int x, y, z, w;
  int highest_w, highest_x, highest_y;
  int num_pairs, num_triples;
  long singles[26];
  long pairs[26][26];
  long triples[26][26][26];
  long highest = -1;
  
  if( argc != 4 ){
    fprintf( stderr, "\nWrong number of arguments\n" );
    fprintf( stderr, "usage: %s filename number_of_pairs number_of_triples\n\n",
             argv[0] );
    exit(1);
  }
  
  sscanf( argv[2], "%d", &num_pairs );
  sscanf( argv[3], "%d", &num_triples );
  
  memset( singles, 0, sizeof(singles) );
  memset( pairs, 0, sizeof(pairs) );
  memset( triples, 0, sizeof(triples) );
  
  infile = fopen( argv[1], "r" );
  
  c1 = next_text_char(infile);
  c2 = next_text_char(infile);

  do{
    c3 = next_text_char(infile);
    
    if( (c1 >= 'a') && (c1 <= 'z') ){
      singles[c1-'a'] += 1;

      if( (c2 >= 'a') && (c2 <= 'z') ){
        pairs[c1-'a'][c2-'a'] += 1;

        if( (c3 >= 'a') && (c3 <= 'z') ){
          triples[c1-'a'][c2-'a'][c3-'a'] += 1;
        }
      }
    }
    
    c1 = c2;
    c2 = c3;
    
  } while( c1 != EOF );
  
  fclose(infile);
  
  printf( " SINGLE CHARACTER FREQUENCIES\n" );
  printf( "------------------------------\n" );

  for( x=0; x<13; x+=1 ){
    printf( " %c %6d\t %c %6d\n", x+'A', singles[x], x+'N', singles[x+13] );
  }
  
  printf( "\n\n %d MOST FREQUENT DIGRAMS\n", num_pairs );
  printf( "--------------------------\n" );

  for( z=0; z<num_pairs; z+=1 ){
    
    for( x=0; x<26; x+=1 ){
      for( y=0; y<26; y+=1 ){
        if( pairs[x][y] > highest ){
          highest = pairs[x][y];
          highest_x = x;
          highest_y = y;
        }
      }
    }

    printf( " %c%c %6d\n", highest_x+'A', highest_y+'A', highest );
    pairs[highest_x][highest_y] = -1;
    highest = -1;
  }
  
  printf( "\n\n %d MOST FREQUENT TRIGRAMS\n", num_triples );
  printf( "---------------------------\n" );

  for( z=0; z<num_triples; z+=1 ){

    for( w=0; w<26; w+=1 ){
      for( x=0; x<26; x+=1 ){
        for( y=0; y<26; y+=1 ){
          if( triples[w][x][y] > highest ){
            highest = triples[w][x][y];
            highest_w = w;
            highest_x = x;
            highest_y = y;
          }
        }
      }
    }
    
    printf( " %c%c%c %6d\n", highest_w+'A', highest_x+'A',
                             highest_y+'A', highest );
    triples[highest_w][highest_x][highest_y] = -1;
    highest = -1;
  }
  
  return(0);
}

int next_text_char( FILE *infile ){
  
  int temp;

  do{
    temp = fgetc( infile );
    if( temp == EOF ){
      break;
    }
    temp = tolower(temp);
  }while( (temp < 'a') || (temp > 'z') );
  
  return( temp );
}
