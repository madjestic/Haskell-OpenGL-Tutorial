#version 150

// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.


// This shader computes the distance to the Mandelbrot Set for everypixel, and colorizes
// it accoringly.
// 
// Z -> Z²+c, Z0 = 0. 
// therefore Z' -> 2·Z·Z' + 1
//
// The Hubbard-Douady potential G(c) is G(c) = log Z/2^n
// G'(c) = Z'/Z/2^n
//
// So the distance is |G(c)|/|G'(c)| = |Z|·log|Z|/|Z'|
//
// More info here: http://www.iquilezles.org/www/articles/distancefractals/distancefractals.htm

in  vec2 fragCoord;
in  float time;
out vec4 fragColor;

void main()
{
  vec3  iResolution = vec3(1024, 1024, 1.0);
  float iGlobalTime = time;
  vec2  p           = -3.0 + 5000.0 * fragCoord.xy / iResolution.xy;
  p.x              *= iResolution.x/iResolution.y;

  // animation	
	float tz = 0.5 - 0.5*cos(0.225*iGlobalTime);
  float zoo = pow( 0.5, 13.0*tz );
	vec2 c = vec2(-0.05,.6805) + p*zoo;

  // iterate
  vec2 z  = vec2(0.0);
  float m2 = 0.0;
  vec2 dz = vec2(0.0);
  for( int i=0; i<256; i++ )
    {
      if( m2>1024.0 ) continue;

      // Z' -> 2·Z·Z' + 1
      dz = 2.0*vec2(z.x*dz.x-z.y*dz.y, z.x*dz.y + z.y*dz.x) + vec2(1.0,0.0);
			
      // Z -> Z² + c			
      z = vec2( z.x*z.x - z.y*z.y, 2.0*z.x*z.y ) + c;
			
      m2 = dot(z,z);
    }

  // distance	
	// d(c) = |Z|·log|Z|/|Z'|
	float d = 0.5*sqrt(dot(z,z)/dot(dz,dz))*log(dot(z,z));

	
  // do some soft coloring based on distance
	d = clamp( 8.0*d/zoo, 0.0, 1.0 );
	d = pow( d, 0.25 );
  vec3 col = vec3( d );
    
  // fragColor = vec4( vec3(fragCoord.x,fragCoord.y,0.0), 1.0 );
  fragColor = vec4( col, 1.0 );
}
