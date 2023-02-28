#!/usr/bin/python

def interpolateTemperatureFrom(self, nodes, distances = False):

    """
    Interpolate temperature from input nodes to actual Nodes instance.
    """

    self.temperature = {}

    # join coordinates and temperature
    try:
        data = [(nodes.coordinates[x],
            nodes.temperature[x]) for x in nodes.coordinates]
    except KeyError as err:
        s = ('There is some node without temperature. %s' % err)
        print(s)
        return s

    points = np.array([x[0] for x in data])
    values = np.array([x[1] for x in data])

    xs = [x[0] for x in points]
    ys = [x[1] for x in points]
    zs = [x[2] for x in points]

    # create large cube around a model
    maxx, maxy, maxz = max(xs), max(ys), max(zs)
    minx, miny, minz = min(xs), min(ys), min(zs)
    avg = np.array([(maxx+minx)/2, (maxy+miny)/2, (maxz+minz)/2])
    cube = 8*[0]
    increase = 20
    cube[0] = increase*(np.array([maxx, maxy, maxz])-avg) + avg
    cube[1] = increase*(np.array([minx, maxy, maxz])-avg) + avg
    cube[2] = increase*(np.array([minx, miny, maxz])-avg) + avg
    cube[3] = increase*(np.array([maxx, miny, maxz])-avg) + avg
    cube[4] = increase*(np.array([maxx, miny, minz])-avg) + avg
    cube[5] = increase*(np.array([minx, miny, minz])-avg) + avg
    cube[6] = increase*(np.array([maxx, maxy, minz])-avg) + avg
    cube[7] = increase*(np.array([minx, maxy, minz])-avg) + avg
    data = [(x, self.coordinates[x]) for x in self.coordinates]
    new_points = [x[1] for x in data]
    ids = [x[0] for x in data]
    mean = np.mean(values)
    cubeValues = [mean,mean,mean,mean,mean,mean,mean,mean]
    points = np.concatenate((points, cube), axis=0)
    values = np.concatenate((values, cubeValues), axis=0)

    grid = griddata(points, values, new_points, method='linear')

    self.temperature = {}
    self.temperature = dict(list(zip(ids, grid)))

    if True or distances:

        tree = cKDTree(points)
        xi = _ndim_coords_from_arrays(new_points, ndim=points.shape[1])
        dists, indexes = tree.query(xi)
        # Copy original result but mask missing values with NaNs
        #grid2 = grid[:]
       # grid2[dists > maxDistance] = np.nan
       # grid = grid2

#        grid = griddata(points, values, new_points, method='linear')
    self.distances = dict(list(zip(ids, dists)))

    return False
