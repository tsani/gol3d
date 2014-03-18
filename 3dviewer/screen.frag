#ifdef GL_ES
precision highp float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

#define CASE(n) if ( i == n )

vec3 getRgbColor( int i )
{
	vec3 result;
	CASE(0) result = vec3( float(  0.0/255.0), float(  0.0/255.0), float(  0.0/255.0)); // black
	CASE(1) result = vec3( float(255.0/255.0), float(255.0/255.0), float(255.0/255.0)); // white
	CASE(2) result = vec3( float(255.0/255.0), float(204.0/255.0), float(204.0/255.0)); // beige
	CASE(3) result = vec3( float(128.0/255.0), float(  0.0/255.0), float(  0.0/255.0)); // brown
	CASE(4) result = vec3( float(255.0/255.0), float(  0.0/255.0), float(  0.0/255.0)); // red
	CASE(5) result = vec3( float(255.0/255.0), float(255.0/255.0), float(  0.0/255.0)); // yellow
	CASE(6) result = vec3( float(  0.0/255.0), float(255.0/255.0), float(  0.0/255.0)); // green
	CASE(7) result = vec3( float(  0.0/255.0), float(255.0/255.0), float(255.0/255.0)); // water
	CASE(8) result = vec3( float(  0.0/255.0), float(  0.0/255.0), float(255.0/255.0)); // blue
	CASE(9) result = vec3( float(128.0/255.0), float(  0.0/255.0), float(128.0/255.0)); // purple
	return result;
}

float circle0(vec2 uv, vec2 pos, float radius)
{
	if(distance(pos, uv) < radius)
	{
		return 1.0;
	}
	
	return 0.0;
}

vec3 circle(vec2 uv, vec2 pos, float radius, vec3 col0, vec3 col1 ) 
{
	float circleMask = circle0(uv, pos, radius);
	vec3 result = mix(col0, col1, circleMask);
	return result;
}

vec3 rect( vec2 pos, float x ,float y, float w, float h, vec3 col0, vec3 col1 )
{
	vec3 result = col0;
	if ( pos.x > x && pos.x < (x + w) 
	  && pos.y > y && pos.y < (y + h) )
	{
		result = col1;
	}
	
	return result;
}

#define PI 3.14159

// from https://glsl.heroku.com/e#15131.0
vec2 nearestHex(float s, vec2 st){
    //float PI = 3.14159265359;
    float TAU = 2.0*PI;
    float deg30 = TAU/12.0;
    float h = sin(deg30)*s;
    float r = cos(deg30)*s;
    float b = s + 2.0*h;
    float a = 2.0*r;
    float m = h/r;

    vec2 sect = st/vec2(2.0*r, h+s);
    vec2 sectPxl = mod(st, vec2(2.0*r, h+s));
    
    float aSection = mod(floor(sect.y), 2.0);
    
    vec2 coord = floor(sect);
    if(aSection > 0.0){
        if(sectPxl.y < (h-sectPxl.x*m)){
            coord -= 1.0;
        }
        else if(sectPxl.y < (-h + sectPxl.x*m)){
            coord.y -= 1.0;
        }

    }
    else{
        if(sectPxl.x > r){
            if(sectPxl.y < (2.0*h - sectPxl.x * m)){
                coord.y -= 1.0;
            }
        }
        else{
            if(sectPxl.y < (sectPxl.x*m)){
                coord.y -= 1.0;
            }
            else{
                coord.x -= 1.0;
            }
        }
    }
    
    float xoff = mod(coord.y, 2.0)*r;
    return vec2(coord.x*2.0*r-xoff, coord.y*(h+s))+vec2(r*2.0, s);
}

vec2 pixel(float s, vec2 st){	
    return ceil(st / s) * s;
}

void main( void ) {

	//vec2 pos = ( gl_FragCoord.xy / resolution.xy );
	//vec2 pos = nearestHex(5.0, gl_FragCoord.xy)/resolution.xy;
	vec2 pos = pixel(5.0, gl_FragCoord.xy)/resolution.xy;

	vec3 col = vec3( 0.0, 0.0, 0.0 );;

	for ( int i = 0; i < 30; i++ ) 
	{
		float x = 0.1 * sin( 2.0 * PI * float(i)/6.0 + time * 0.5) + 0.5;
		float y = 0.1 * cos( 2.0 * PI * float(i)/6.0 + time * 0.5) + 0.5;
		
		//col = rect( pos, float(i)/9.0, 0.5, 0.1, 0.1, col, getRgbColor(i) );
		//col = rect( pos, x, y, 0.1, 0.1, col, getRgbColor(int(mod(float(i),10.0))) );
		col = circle( pos, vec2(x, y), 0.05, col, getRgbColor(int(mod(float(i),10.0))) );
	}

	gl_FragColor = vec4( col, 1.0 );
}
