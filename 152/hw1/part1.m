P1 = [-1 -1 0];
P2 = [1 -1 0];
P3 = [1 1 0];
P4 = [-1 1 0];
O = [0 .5 2];
f = 1

% a)
m_in = [f 0 0 ; 0 f 0 ; 0 0 1]

% b) rotation
r = [(cos(-pi/4)) (0) (sin(-pi/4)) ; (-sin(pi/4) * (-sin(-pi/4))) (cos(pi/4)) (-sin(pi/4) * cos(-pi/4)) ; (cos(pi/4) * (-sin(-pi/4))) (sin(pi/4)) (cos(pi/4) * cos(-pi/4))]

% c)
m_ex = [r O']

P1_a = [m_ex ; 0 0 0 1] * [P1 1]'
P2_a = [m_ex ; 0 0 0 1] * [P2 1]'
P3_a = [m_ex ; 0 0 0 1] * [P3 1]'
P4_a = [m_ex ; 0 0 0 1] * [P4 1]'

% d)
m = m_in * m_ex
P1_b = m * [P1 1]'
P2_b = m * [P2 1]'
P3_b = m * [P3 1]'
P4_b = m * [P4 1]'

p1 = [(P1_b(1) / P1_b(3)) (P1_b(2) / P1_b(3))]
p2 = [(P2_b(1) / P2_b(3)) (P2_b(2) / P2_b(3))]
p3 = [(P3_b(1) / P3_b(3)) (P3_b(2) / P3_b(3))]
p4 = [(P4_b(1) / P4_b(3)) (P4_b(2) / P4_b(3))]

pdepoly([p1(1) p2(1) p3(1) p4(1)], [p1(2) p2(2) p3(2) p4(2)])

% e)
l1 = cross(P1_b, P2_b);
l2 = cross(P4_b, P3_b);
v1_h = cross(l1, l2);
v1 = [v1_h(1) / v1_h(3) v1_h(2) / v1_h(3)]

l1 = cross(P2_b, P3_b);
l2 = cross(P1_b, P4_b);
v2_h = cross(l1, l2);
v2 = [v2_h(1) / v2_h(3) v2_h(2) / v2_h(3)]

% f)
m_aff = m;
m_aff(3,1) = 0;
m_aff(3,2) = 0;
m_aff(3,3) = 0;

P1_a_aff = m_aff * [P1 1]'
P2_a_aff = m_aff * [P2 1]'
P3_a_aff = m_aff * [P3 1]'
P4_a_aff = m_aff * [P4 1]'

p1_aff = [(P1_a_aff(1) / P1_a_aff(3)) (P1_a_aff(2) / P1_a_aff(3))]
p2_aff = [(P2_a_aff(1) / P2_a_aff(3)) (P2_a_aff(2) / P2_a_aff(3))]
p3_aff = [(P3_a_aff(1) / P3_a_aff(3)) (P3_a_aff(2) / P3_a_aff(3))]
p4_aff = [(P4_a_aff(1) / P4_a_aff(3)) (P4_a_aff(2) / P4_a_aff(3))]

pdepoly([p1_aff(1) p2_aff(1) p3_aff(1) p4_aff(1)], [p1_aff(2) p2_aff(2) p3_aff(2) p4_aff(2)])

% g)
x0 = O(1);
y0 = O(2);
z0 = O(3);

m_ort = [f/z0 0 -(x0/z0*z0) x0/z0 ; 0 f/z0 -(y0/z0*z0) y0/z0 ; 0 0 0 1]
P1_a_ort = m_ort * [P1 1]'
P2_a_ort = m_ort * [P2 1]'
P3_a_ort = m_ort * [P3 1]'
P4_a_ort = m_ort * [P4 1]'

p1_ort = [(P1_a_ort(1) / P1_a_ort(3)) (P1_a_ort(2) / P1_a_ort(3))]
p2_ort = [(P2_a_ort(1) / P2_a_ort(3)) (P2_a_ort(2) / P2_a_ort(3))]
p3_ort = [(P3_a_ort(1) / P3_a_ort(3)) (P3_a_ort(2) / P3_a_ort(3))]
p4_ort = [(P4_a_ort(1) / P4_a_ort(3)) (P4_a_ort(2) / P4_a_ort(3))]

pdepoly([p1_ort(1) p2_ort(1) p3_ort(1) p4_ort(1)], [p1_ort(2) p2_ort(2) p3_ort(2) p4_ort(2)])

% The affine and orthographic seems to be more appropriate when using the full perspective
% camera matrix. The projections onto the image plane are similar to the projection using
% the full prospective camera matrix. Since the focal is multiply by 10 the affine and full
% projections are carrying about distance. This is why we can not see them. At the oppsosite,
% the orthographic projection do not care about distances and show the same as before.
