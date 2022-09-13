def rquad(N, k):(
        k1=k+1
        k2=k+2

        n=list(range(1:N))
        nnk=2*n+k

        A=[k/k2 repmat(k^2,1,N)./(nnk.*(nnk+2))];
        n=2:N;
        nnk=nnk(n);

        B1=4*k1/(k2*k2*(k+3));
        nk=n+k;
        nnk2=nnk.*nnk;

        B=4*(n.*nk).^2./(nnk2.*nnk2-nnk2);

        ab=[A' [(2^k1)/k1;
            B1;
            B']];
        s=sqrt(ab(2:N,2));

        [V,X]=eig(diag(ab(1:N,1),0)+diag(s,-1)+diag(s,1));

        [X,I]=sort(diag(X));

        % Grid points
        x=(X+1)/2;

        % Quadrature weights
        w=(1/2)^(k1)*ab(1,2)*V(1,I)'.^2;


def tetraquad(N, vert):

    q1, w1 = rquad(N, 2)
    q2, w2 = rquad(N, 1)
    q3, w3 = rquad(N, 0)

    [q1,q2,q3]=meshgrid(q1,q2,q3);
    q1=q1(:);
    q2=q2(:);
    q3=q3(:);

    x=1-q1;
    y=(1-q2).*q1;
    z=q1.*q2.*q3;
