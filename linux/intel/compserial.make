#
#
#  ##############################################################
#  ##                                                          ##
#  ##  compserial.make  --  compile Tinker modules for serial  ##
#  ##             (Intel Fortran for Linux Version)            ##
#  ##                                                          ##
#  ##############################################################
#
#
#  compile all the modules; "sizes" must be first since it is used
#  to set static array dimensions in many of the other modules
#
#
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 sizes.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 action.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 align.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 analyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 angang.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 angbnd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 angpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 angtor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 argue.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ascii.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 atmlst.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 atomid.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 atoms.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bath.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bitor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bndpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bndstr.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bound.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 boxes.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 cell.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 cflux.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 charge.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chgpen.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chgpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chgtrn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chrono.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chunks.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 couple.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ctrpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 deriv.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 dipole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 disgeo.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 disp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 dma.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 domega.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 dsppot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 energi.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ewald.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 faces.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 fft.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 fields.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 files.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 fracs.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 freeze.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 gkstuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 group.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 hescut.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 hessn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 hpmf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ielscf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 improp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 imptor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 inform.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 inter.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 iounit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kanang.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kangs.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kantor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 katoms.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kbonds.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kcflux.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kchrge.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kcpen.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kctrn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kdipol.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kdsp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 keys.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 khbond.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kiprop.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kitors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kmulti.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kopbnd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kopdst.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 korbs.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kpitor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kpolr.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 krepl.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ksolut.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kstbnd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ksttor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ktorsn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ktrtor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kurybr.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kvdwpr.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kvdws.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 light.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 limits.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 linmin.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 math.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mdstuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 merck.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 minima.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 molcul.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 moldyn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 moment.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mplpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mpole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mrecip.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mutant.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 neigh.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nonpol.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nucleo.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 omega.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 opbend.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 opdist.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 openmp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 orbits.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 output.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 params.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 paths.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pbstuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pdb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 phipsi.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 piorbs.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pistuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pitors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pme.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 polar.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 polgrp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 polopt.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 polpcg.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 polpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 poltcg.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 potent.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 potfit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ptable.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 qmstuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 refer.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 repel.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 reppot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 resdue.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 restrn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rgddyn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rigid.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ring.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rotbnd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rxnfld.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rxnpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 scales.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 sequen.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 shunt.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 socket.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 solpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 solute.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 stodyn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 strbnd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 strtor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 syntrn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 tarray.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 titles.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 torpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 tors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 tortor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 tree.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 units.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 uprior.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 urey.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 urypot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 usage.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 valfit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 vdw.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 vdwpot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 vibs.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 virial.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 warp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xtals.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 zclose.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 zcoord.f
#
#  now compile separately each of the Fortran source files
#
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 active.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 alchemy.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 alterchg.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 analysis.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 analyze.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 angles.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 anneal.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 archive.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 attach.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 baoab.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bar.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 basefile.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 beeman.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bicubic.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bitors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bonds.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 born.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bounds.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 bussi.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 calendar.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 center.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chkpole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chkring.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 chkxyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 cholesky.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 clock.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 cluster.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 column.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 command.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 connect.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 connolly.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 control.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 correlate.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 crystal.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 cspline.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 cutoffs.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 damping.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 dcflux.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 deflate.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 delete.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 diagq.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 diffeq.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 diffuse.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 distgeom.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 document.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 dynamic.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangang.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangang1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangang2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangang3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangle.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangle1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangle2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangle3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangtor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangtor1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangtor2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eangtor3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebond.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebond1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebond2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebond3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebuck.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebuck1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebuck2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ebuck3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echarge.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echarge1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echarge2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echarge3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgdpl.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgdpl1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgdpl2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgdpl3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgtrn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgtrn1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgtrn2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 echgtrn3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edipole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edipole1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edipole2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edipole3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edisp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edisp1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edisp2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 edisp3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egauss.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egauss1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egauss2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egauss3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egeom.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egeom1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egeom2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 egeom3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ehal.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ehal1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ehal2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ehal3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimprop.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimprop1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimprop2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimprop3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimptor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimptor1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimptor2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eimptor3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 elj.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 elj1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 elj2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 elj3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 embed.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emetal.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emetal1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emetal2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emetal3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emm3hb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emm3hb1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emm3hb2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 emm3hb3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 empole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 empole1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 empole2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 empole3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 energy.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopbend.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopbend1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopbend2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopbend3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopdist.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopdist1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopdist2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eopdist3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epitors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epitors1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epitors2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epitors3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epolar.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epolar1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epolar2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 epolar3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erepel.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erepel1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erepel2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erepel3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erxnfld.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erxnfld1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erxnfld2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 erxnfld3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 esolv.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 esolv1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 esolv2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 esolv3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrbnd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrbnd1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrbnd2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrbnd3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrtor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrtor1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrtor2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 estrtor3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etors1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etors2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etors3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etortor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etortor1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etortor2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 etortor3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eurey.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eurey1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eurey2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 eurey3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 evcorr.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 extra.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 extra1.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 extra2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 extra3.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 fatal.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 fft3d.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 fftpack.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 field.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 final.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 flatten.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 freeunit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 gda.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 geometry.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getarc.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getint.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getkey.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getmol.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getmol2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getnumb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getpdb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getprm.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getref.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getstring.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 gettext.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getword.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 getxyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ghmcstep.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 gradient.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 gradrgd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 gradrot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 groups.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 grpline.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 gyrate.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 hessian.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 hessrgd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 hessrot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 hybrid.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 image.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 impose.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 induce.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 inertia.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 initatom.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 initial.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 initprm.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 initres.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 initrot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 insert.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 intedit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 intxyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 invbeta.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 invert.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 jacobi.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kangang.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kangle.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kangtor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 katom.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kbond.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kcharge.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kchgflx.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kchgtrn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kdipole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kdisp.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kewald.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kextra.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kgeom.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kimprop.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kimptor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kinetic.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kmetal.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kmpole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kopbend.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kopdist.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 korbit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kpitors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kpolar.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 krepel.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ksolv.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kstrbnd.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kstrtor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ktors.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ktortor.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kurey.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 kvdw.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 lattice.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 lbfgs.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 lights.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 lusolve.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 makeint.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 makeref.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 makexyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 maxwell.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mdinit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mdrest.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mdsave.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mdstat.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mechanic.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 merge.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 minimize.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 minirot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 minrigid.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mol2xyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 molecule.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 molxyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 moments.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 monte.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 mutate.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nblist.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 newton.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 newtrot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nextarg.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nexttext.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nose.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nspline.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 nucleic.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 number.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 numeral.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 numgrad.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 ocvm.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 openend.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 optimize.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 optinit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 optirot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 optrigid.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 optsave.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 orbital.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 orient.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 orthog.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 overlap.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 path.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pdbxyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 picalc.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pmestuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pmpb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 polarize.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 poledit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 polymer.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 potential.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 predict.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pressure.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prmedit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prmkey.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 promo.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 protein.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prtdyn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prterr.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prtint.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prtmol2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prtpdb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prtprm.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prtseq.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 prtxyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pss.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pssrigid.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 pssrot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 qrsolve.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 quatfit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 radial.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 random.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rattle.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readdyn.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readgau.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readgdma.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readint.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readmol.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readmol2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readpdb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readprm.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readseq.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 readxyz.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 replica.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 respa.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rgdstep.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rings.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rmsfit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rotlist.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 rotpole.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 saddle.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 scan.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 sdstep.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 search.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 server.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 shakeup.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 sigmoid.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 simplex.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 sktstuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 sniffer.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 sort.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 spacefill.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 spectrum.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 square.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 suffix.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 superpose.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 surface.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 surfatom.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 switch.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 tcgstuf.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 temper.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 testgrad.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 testhess.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 testpair.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 testpol.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 testrot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 testvir.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 timer.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 timerot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 tncg.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 torphase.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 torque.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 torsfit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 torsions.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 trimtext.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 unitcell.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 valence.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 verlet.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 version.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 vibbig.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 vibrate.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 vibrot.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 volume.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xtalfit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xtalmin.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xyzatm.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xyzedit.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xyzint.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xyzmol2.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 xyzpdb.f
ifort -c -O3 -axSSSE3 -no-ipo -no-prec-div -vec-report0 zatom.f
