#define M_PI 3.1415926535897932384626433832795

float s1  = 1.0f;
float s2  = 10.0f;
float far = 2000.0f;

float f1 (in float x, in float s)
{
	return (-s*log (x));
}

float f2 (in float x, in float s)
{
	return (-pow(s*log (x), 2.0f));
}

float mixF (in float x, in float far)
{
	return (cos (clamp (x/(-far) + M_PI, 0.0f, M_PI)) + 1.0f);
}

float log10(in float x)
{
	float far  = 10.0f;
	float result = (10.0f / log(far)) * log(x);
	return(result);
}

// Angle-Vector rotation with a Matrix
vec2 rotate(vec2 v, float a) {
	float s = sin(a);
	float c = cos(a);
	mat2 m = mat2(c, -s, s, c);
	return m * v;
}

mat4 rotationMatrix(vec3 axis, float angle) {
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;
    
    return mat4(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,  0.0,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,  0.0,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c,           0.0,
                0.0,                                0.0,                                0.0,                                1.0);
}

vec3 rotate(vec3 v, vec3 axis, float angle) {
	mat4 m = rotationMatrix(axis, angle);
	return (m * vec4(v, 1.0)).xyz;
}

// Rotations with a Quaterniong

vec4 quat_from_axis_angle(vec3 axis, float angle)
{ 
  vec4 qr;
  float half_angle = (angle * 0.5) * 3.14159 / 180.0;
  qr.x = axis.x * sin(half_angle);
  qr.y = axis.y * sin(half_angle);
  qr.z = axis.z * sin(half_angle);
  qr.w = cos(half_angle);
  return qr;
}

vec4 quat_conj(vec4 q)
{ 
  return vec4(-q.x, -q.y, -q.z, q.w); 
}
  
vec4 quat_mult(vec4 q1, vec4 q2)
{ 
  vec4 qr;
  qr.x = (q1.w * q2.x) + (q1.x * q2.w) + (q1.y * q2.z) - (q1.z * q2.y);
  qr.y = (q1.w * q2.y) - (q1.x * q2.z) + (q1.y * q2.w) + (q1.z * q2.x);
  qr.z = (q1.w * q2.z) + (q1.x * q2.y) - (q1.y * q2.x) + (q1.z * q2.w);
  qr.w = (q1.w * q2.w) - (q1.x * q2.x) - (q1.y * q2.y) - (q1.z * q2.z);
  return qr;
}

// vec3 rotate_by_vec3_angle(vec3 pos, vec3 axis, float angle)
// { 
//   vec4 qr = quat_from_axis_angle(axis, angle);
//   vec4 qr_conj = quat_conj(qr);
//   vec4 q_pos = vec4(pos.x, pos.y, pos.z, 0);
  
//   vec4 q_tmp = quat_mult(qr, q_pos);
//   qr = quat_mult(q_tmp, qr_conj);
  
//   return vec3(qr.x, qr.y, qr.z);
// }

vec3 rotate_by_vec3_angle(vec3 position, vec3 axis, float angle)
{ 
  vec4 qr = quat_from_axis_angle(axis, angle);
  vec4 qr_conj = quat_conj(qr);
  vec4 q_pos = vec4(position.x, position.y, position.z, 0);
  
  vec4 q_tmp = quat_mult(qr, q_pos);
  qr = quat_mult(q_tmp, qr_conj);
  
  return vec3(qr.x, qr.y, qr.z);
}

vec4 mult_quat (vec4 pos, vec4 quat)
{
  float angle = quat.w;
  vec3 axis   = quat.xyz;
  vec4 q      = quat_from_axis_angle(axis, angle);
  vec3 v      = pos.xyz;
  return vec4 (v + 2.0 * cross(q.xyz, cross(q.xyz, v) + q.w * v), pos.w);
}
