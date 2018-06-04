FROM budgetapp-dep
COPY . /opt/build
WORKDIR /opt/budget
RUN cd /opt/build && stack build --no-system-ghc --fast
RUN ls -RA /opt/build | grep bin
RUN cp -R /opt/build/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/bin /opt/budget/
RUN chmod +x /opt/budget/bin/budget-exe
