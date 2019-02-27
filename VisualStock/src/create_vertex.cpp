//  create_vertex.cpp
//  "create_vertex" takes the merge matrix (n-1 by 2) and a row number (counting from 1) i.
//  "create_vertex" find all points belong to the vertex containing points in row i and all related (points in row smaller than i and merged with points in row i).
//  "create_vertex returns" two vectors: rows_checked (during this points search process) and points_contained (in this vertex).
//  "create_vertex_pre" is the function does real calculation: updating  the vectors rows_checked and points_contained from a default initialized vectors.

#include <Rcpp.h>
using namespace Rcpp;

void create_vertex_pre(IntegerMatrix& merge_mat, int i, std::vector<int>& rows_checked, std::vector<int>& points_contained)
{
    int j = i - 1; //convert matrix from 1~n by 1~n to 0~n-1 by 0~n-1
    if (merge_mat(j, 0) > 0 && merge_mat(j, 1) > 0)
    {
        rows_checked.push_back(merge_mat(j, 0));
        create_vertex_pre(merge_mat, merge_mat(j, 0), rows_checked, points_contained);
        rows_checked.push_back(merge_mat(j, 1));
        create_vertex_pre(merge_mat, merge_mat(j, 1), rows_checked, points_contained);
    } else if (merge_mat(j, 0) < 0 && merge_mat(j, 1) > 0){
        points_contained.push_back(abs(merge_mat(j, 0)));
        rows_checked.push_back(merge_mat(j, 1));
        create_vertex_pre(merge_mat, merge_mat(j, 1), rows_checked, points_contained);
    } else if (merge_mat(j, 0) < 0 && merge_mat(j, 1) < 0){
         points_contained.push_back(abs(merge_mat(j, 0)));
         points_contained.push_back(abs(merge_mat(j, 1)));
    }
}

// [[Rcpp::export]]
List create_vertex(IntegerMatrix& merge_mat, int i)
{
    std::vector<int> rows_checked;
    std::vector<int> points_contained;
    create_vertex_pre(merge_mat, i, rows_checked, points_contained);
    return List::create(_["rows_checked"] = rows_checked, _["points_contained"] = points_contained);
}
