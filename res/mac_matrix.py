#!/usr/bin/python3

import numpy as np
import random as rnd
import matplotlib.pyplot as plt


def _generate_data(numnodes=10, numfreqs=10):
    data = {}

    for f in range(numfreqs):
        data[f+1] = {}
        x = np.random.random(numnodes).flatten()
        y = np.random.random(numnodes).flatten()
        z = np.random.random(numnodes).flatten()

        for n in range(numnodes):
            data[f+1][n+1] = [float(x[n]) * (2 * rnd.getrandbits(1) - 1),
                              float(y[n]) * (2 * rnd.getrandbits(1) - 1),
                              float(z[n]) * (2 * rnd.getrandbits(1) - 1)]
    return data


def _generate_data_matrix(numnodes=9, numfreqs=9):
    '''
    numfreqs must be a multiple of 3
    '''
    shape = max(numfreqs, numnodes * 3)
    A = np.random.random((shape, shape))
    A = 0.5 * (A + A.T)
    # A = A @ A.T
    A += np.eye(shape)

    w, v = np.linalg.eig(A)

    idxs = np.argsort(w)

    data = {}
    for i, idx in enumerate(idxs):
        if i == numfreqs:
            break
        data[w[idx]] = {}
        for n in range(int(numnodes)):
            data[w[idx]][(n*3)+1] = [v[(n*3),idx], v[(n*3)+1,idx], v[(n*3)+2,idx]]
    return data


def _dict2vector(data):
    return np.array([v for k, v in data.items()]).flatten()


def MAC(bfreqData, cfreqData):
    A = _dict2vector(bfreqData)
    B = _dict2vector(cfreqData)

    mac = (np.dot(A, B) ** 2) / (np.dot(A, A) * np.dot(B, B))
    # print(f'{A = }')
    # print(f'{B = }')
    # print(f'{mac = }')

    return mac


def MACMatrix(bData, cData):
    bFreqs = list(bData.keys())
    cFreqs = list(cData.keys())

    macM = []
    for bFreq in bFreqs:
        bFreqData = bData[bFreq]
        v = []
        for cFreq in cFreqs:
            cFreqData = cData[cFreq]
            v.append(MAC(bFreqData, cFreqData))
        macM.append(v)
    return np.array(macM)


def MACplot(macM, bFreqs, cFreqs):
    x = np.array(cFreqs)
    y = np.array(bFreqs)
    x,y = np.meshgrid(x, y)

    fig, ax = plt.subplots()

    cf = ax.contourf(x, y, macM, cmap=plt.cm.coolwarm)

    plt.show()


def MACshow(macM, bFreqs, cFreqs):
    x = np.array(cFreqs)
    y = np.flip(np.array(bFreqs))
    print(f'{x.shape[0] = }')

    fig, ax = plt.subplots()

    cf = ax.matshow(np.flip(macM, axis=0), cmap=plt.cm.coolwarm)

    _x_labels = lambda xcoor, pos: f'{x[max(0, min(round(xcoor), x.shape[0]-1))]:.2f}'
    _y_labels = lambda ycoor, pos: f'{y[max(0, min(round(ycoor), y.shape[0]-1))]:.2f}'

    # ax.set_xticks(list(range(len(x))))
    # ax.set_xticklabels(x)
    ax.xaxis.set_ticks_position('bottom')
    ax.xaxis.set_major_formatter(_x_labels)

    # ax.set_yticks(list(range(len(y))))
    # ax.set_yticklabels(y)
    ax.yaxis.set_major_formatter(_y_labels)

    plt.show()


if __name__ == '__main__':
    # bData = _generate_data(numnodes=10, numfreqs=100)
    # cData = _generate_data(numnodes=10, numfreqs=100)
    bData = _generate_data_matrix(numnodes=9, numfreqs=12)
    cData = _generate_data_matrix(numnodes=9, numfreqs=120)
    macM = MACMatrix(bData, cData)
    MACshow(macM, list(bData.keys()), list(cData.keys()))

