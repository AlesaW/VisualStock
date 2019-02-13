//
//  Find_points.cpp
//  
//
//  Created by Alesa on 2/11/19.
//

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
