um = array(data = NA, dim = 5)
vm = array(data = NA, dim = 5)
wm = array(data = NA, dim = 5)
tm = array(data = NA, dim = 5)
z = matrix(data = NA, nrow = 30010, ncol = 5)
up = matrix(data = NA, nrow = 30010, ncol = 5)
vp = matrix(data = NA, nrow = 30010, ncol = 5)
wp = matrix(data = NA, nrow = 30010, ncol = 5)
t = matrix(data = NA, nrow = 30010, ncol = 5)

uw = array(data = NA, dim = 5)
vw = array(data = NA, dim = 5)
uv = array(data = NA, dim = 5)
u2 = array(data = NA, dim = 5)
v2 = array(data = NA, dim = 5)
w2 = array(data = NA, dim = 5)
u3 = array(data = NA, dim = 5)
v3 = array(data = NA, dim = 5)
w3 = array(data = NA, dim = 5)
u4 = array(data = NA, dim = 5)
v4 = array(data = NA, dim = 5)
w4 = array(data = NA, dim = 5)
corr = array(data = NA, dim = 5)

i = 2
#ciclo sulle quote
for (j in 1:5) {
  print(i)
  z[j] = i
  if (j <= 3) {
    name = paste("Trial46/sonic_center_tower_0", i, sep = "", "m_Trial46.dat")
  } else {
    name = paste("Trial46/sonic_center_tower_", i, sep = "", "m_Trial46.dat")
  }
  print(name)
  sonic = read.table(name, skip = 70, sep = ",")
  um[j] = mean(sonic[, 4])
  vm[j] = mean(sonic[, 5])
  wm[j] = mean(sonic[, 6])
  tm[j] = mean(sonic[, 7])
  t[, j] = sonic[, 7]

  up[, j] = sonic[, 4] - um[j]
  vp[, j] = sonic[, 5] - vm[j]
  wp[, j] = sonic[, 6] - wm[j]

  u2[j] = mean(up[, j]^2)
  v2[j] = mean(vp[, j]^2)
  w2[j] = mean(wp[, j]^2)

  u3[j] = mean(up[, j]^3)
  v3[j] = mean(vp[, j]^3)
  w3[j] = mean(wp[, j]^3)

  u4[j] = mean(up[, j]^4)
  v4[j] = mean(vp[, j]^4)
  w4[j] = mean(wp[, j]^4)

  uw[j] = mean(up[, j] * wp[, j])
  vw[j] = mean(vp[, j] * wp[, j])
  uv[j] = mean(up[, j] * vp[, j])
  i = 2 * i
}
ust = (uw^2 + vw^2)^(1 / 4)
q2 = u2 + v2 + w2

#for (j in 1:length(wp[,1])){
for (l in 1:1000) {
  ll = l * 3
  if (l %% 10 == 0) { print(l) }
  #  for (l in 1:(length(wp[,1])/2)){
  for (j in 1:5000) {

    corr[j] = mean(up[j + l, 4] * up[j, 4])
  } }

par(mfrow = c(2, 2))
plot(sqrt(um^2 + vm^2), z, type = "l", xlim = c(0, 10), ylim = c(0, 50))
plot(u2 / (ust * ust), z, type = "l", xlim = c(0, 15), ylim = c(0, 50))
plot(v2 / (ust * ust), z, type = "l", xlim = c(0, 20), ylim = c(0, 50))
plot(w2 / (ust * ust), z, type = "l", xlim = c(0, 2), ylim = c(0, 50))


par(mfrow = c(1, 3))
plot(u3 / (ust^3), z, type = "l", xlim = c(-8, 0), ylim = c(0, 50))
plot(v3 / (ust^3), z, type = "l", xlim = c(0, 100), ylim = c(0, 50))
plot(w3 / (ust^3), z, type = "l", xlim = c(-0.5, 0.5), ylim = c(0, 50))

par(mfrow = c(1, 3))
plot(u4 / (ust^4), z, type = "l", xlim = c(0, 100), ylim = c(0, 50))
plot(v4 / (ust^4), z, type = "l", xlim = c(0, 2100), ylim = c(0, 50))
plot(w4 / (ust^4), z, type = "l", xlim = c(0, 6), ylim = c(0, 50))

par(mfrow = c(2, 2))
plot(uw / (ust^2), z, type = "l", xlim = c(-1, 1), ylim = c(0, 50))
plot(vw / (ust^2), z, type = "l", xlim = c(-1, 0), ylim = c(0, 50))
plot(uv / (ust^2), z, type = "l", xlim = c(0, 8), ylim = c(0, 50))
plot(ust, z, type = "l", xlim = c(-1, 1), ylim = c(0, 50))

ust = 0.27
c0s = 2
czs = c(0, 1, 2, 3, 4, 5, 10, 25, 30, 40, 50, 60, 80, 100)
sigmau = ust * 2. * exp(-3. * 0.0001 * czs / ust)
sigmaw = 0.2 * ust * 1.3 * exp(-2. * 0.0001 * czs / ust) #*0.2
sigmav = sigmaw / 0.2                          #/0.2
tu = 0.5 * (czs / sigmaw) / (1 + 15 * 0.0001 * czs / ust)
tv = tu
tw = tu


eps = sigmaw * sigmaw * sigmaw / 2.
tu = 2 * (sigmau)**2 / (c0s * eps)
tv = 2 * (sigmav)**2 / (c0s * eps)
tw = 2 * (sigmaw)**2 / (c0s * eps)

for (l in 3601:30010) {
  time = 3601 +
    if (l %% 10 == 0) { print(l) }
  #  for (l in 1:(length(wp[,1])/2)){
  for (j in 1:5000) {

    corr[j] = mean(up[j + l, 4] * up[j, 4])
  } }
