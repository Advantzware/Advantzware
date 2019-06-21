using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;

public partial class show_avail_order_entry : System.Web.UI.Page
{
   
    
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {

            mod_name_label.Text = Convert.ToString(Session["sub_prgrm_url"]);
            user_name_label.Text = UserLogin.UserName;
        }
        catch { }
        CreateRecordBound();
        BuidDataSource();
        grid2databind();
        //Response.Write(Session["show_avail_order_seq_grid2_index"]);
        try
        {
            if (Session["show_avail_order_seq_grid2_index"] != null)
            {
                GridView2.SelectedIndex = Convert.ToInt32(Session["show_avail_order_seq_grid2_index"]);
                Session["show_avail_order_seq_grid2"] = ((Label)GridView2.SelectedRow.FindControl("seq_label")).Text;
            }
            if (Session["show_avail_order_seq_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["show_avail_order_seq_index"]);
                Session["show_avail_order_seq"] = ((Label)GridView1.SelectedRow.FindControl("seq_label")).Text;
            }
        }
        catch { }
        try
        {            
            if (Session["show_avail_order_seq_index"] == null)
            {
                GridView1.SelectedIndex = 0;                
                Session["show_avail_order_seq"] = ((Label)GridView1.SelectedRow.FindControl("seq_label")).Text;                
            }
            if (Session["show_avail_order_seq_grid2_index"] == null)
            {
                GridView2.SelectedIndex = 0;
                Session["show_avail_order_seq_grid2"] = ((Label)GridView2.SelectedRow.FindControl("seq_label")).Text;
            }
        }
        catch { }
        
        
        
    }
    private void BuidDataSource()
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select seq_no, col_name,col_seq from column_seq_user where sub_program = '" + Convert.ToString(Session["sub_prgrm_url"]) + "' and main_program = '" + Convert.ToString(Session["main_prgrm_url"]) + "' and user_name = '" + UserLogin.UserName + "' and display = 1  order by seq_no  ";
            
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            
            GridView1.DataSource = ds;
            GridView1.DataBind();

        }
        catch (Exception ex)
        {
            lbl_error.Text = "Session Expired! Please relogin";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }
    }

    private void grid2databind()
    {

        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select seq_no, col_name,col_seq from column_seq_user where sub_program = '" + Convert.ToString(Session["sub_prgrm_url"]) + "' and main_program = '" + Convert.ToString(Session["main_prgrm_url"]) + "' and user_name = '" + UserLogin.UserName + "' and display = 0  order by col_seq  ";
            
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            GridView2.DataSource = ds;
            GridView2.DataBind();

        }
        catch (Exception ex)
        {
            lbl_error.Text = "No Record Found";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }
    }

    private void CreateRecordBound()
    {
        UserClass UserLogin = (UserClass)Session["User"];
         SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
         try
         {
             conn.Open();

             string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and sub_program = '" + Convert.ToString(Session["sub_prgrm_url"]) + "' ";
             SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
             DataSet ds = new DataSet();
             da.Fill(ds);

             if (ds.Tables[0].Rows.Count == 0)
             {
                 string cmd2 = "select col_name ,seq_no , col_val   from column_maintenance where sub_program = '" + Convert.ToString(Session["sub_prgrm_url"]) + "' order by seq_no  ";
                 SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
                 DataSet ds2 = new DataSet();
                 da2.Fill(ds2);
                 int seq = 1;
                 int row = 0;
                 for (int i = 0; i < ds2.Tables[0].Rows.Count; i++)
                 {

                     SqlCommand cmd_insert = new SqlCommand("insert into column_seq_user (user_name, main_program, sub_program, col_name, col_val, seq_no, col_seq, display ) values ('" + UserLogin.UserName + "','" + Convert.ToString(Session["main_prgrm_url"]) + "','" + Convert.ToString(Session["sub_prgrm_url"]) + "', '" + ds2.Tables[0].Rows[row][0].ToString() + "', '" + ds2.Tables[0].Rows[row][2].ToString() + "','" + ds2.Tables[0].Rows[row][1].ToString() + "','" + seq + "', '" + 0 + "')", conn);
                     cmd_insert.ExecuteNonQuery();


                     row = row + 1;
                     seq = seq + 1;
                 }
                 conn.Close();
             }
         }
         catch (Exception ex)
         {
             lbl_error.Text = ex.Message;
             conn.Close();
         }
         finally
         {
             conn.Close();
         }

         
    }

    protected void add_button_click(object sender, ImageClickEventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            //string cmd = "select main_program, sub_program, col_name, col_val from column_maintenance where seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq"]) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "'";
            //SqlDataAdapter dap = new SqlDataAdapter(cmd, conn);
            //DataSet das = new DataSet();
            //dap.Fill(das);

            //int gridcount = GridView2.Rows.Count;
            //gridcount = gridcount + 1;
            //int col_seq_no = 0;
            try
            {
                //string cmd2 = "select seq_no from column_seq_user where  seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq"]) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "'";
                //SqlDataAdapter dap2 = new SqlDataAdapter(cmd2, conn);
                //DataSet das2 = new DataSet();
                //dap2.Fill(das2);
                //col_seq_no = Convert.ToInt32(das2.Tables[0].Rows[0][0].ToString());
                //Response.Write(GridView2.Rows.Count);
                Int32 col_seq = GridView2.Rows.Count + 1;

                SqlCommand cmd_update = new SqlCommand("update column_seq_user set display = 0, col_seq = '" + col_seq + "'  where user_name = '" + UserLogin.UserName + "' and seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq"]) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' ", conn);
                cmd_update.ExecuteNonQuery();

                BuidDataSource();
                grid2databind();
                conn.Close();
            }
            catch
            {
                //col_seq_no = 0;   
            }

            //if (col_seq_no == 0)
            //{
            //    SqlCommand cmd_insert = new SqlCommand("insert into column_seq_user(seq_no, main_program,sub_program,col_name, col_val,user_name,col_seq ) values ('" + Convert.ToInt32(Session["show_avail_order_seq"]) + "','" + Convert.ToString(Session["main_prgrm_url"]) + "','" + Convert.ToString(Session["sub_prgrm_url"]) + "','" + das.Tables[0].Rows[0][2].ToString().Trim() + "','" + das.Tables[0].Rows[0][3].ToString().Trim() + "','" + UserLogin.UserName + "','" + gridcount + "')", conn);
            //    cmd_insert.ExecuteNonQuery();
            //    SqlCommand cmd_update = new SqlCommand("update column_maintenance set display = 0  where seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq"]) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' ", conn);
            //    cmd_update.ExecuteNonQuery();
            //    conn.Close();
            //    BuidDataSource();
            //    grid2databind();
            //}
            //if (col_seq_no > 0)
            //{
            //    lbl_error.Text = "Record already exist";
            //}
            

        }
        catch (Exception ex)
        {
            lbl_error.Text = ex.Message;
            conn.Close();
        }
        finally
        {
            conn.Close();
        }

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["show_avail_order_seq_index"] = GridView1.SelectedIndex;
        Session["show_avail_order_seq"] = ((Label)GridView1.SelectedRow.FindControl("seq_label")).Text;
        
    }
    protected void delete_button_click(object sender,ImageClickEventArgs e)
    {
        //int col_seq_no = 0;
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        UserClass UserLogin = (UserClass)Session["User"];
        //string cmd2 = "select col_seq from column_seq_user where  seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq_grid2"]) + "' and sub_program = '" + Convert.ToString(Session["sub_prgrm_url"]) + "'";
        //SqlDataAdapter dap2 = new SqlDataAdapter(cmd2, conn);
        //DataSet das2 = new DataSet();
        //dap2.Fill(das2);
        //col_seq_no = Convert.ToInt32(das2.Tables[0].Rows[0][0].ToString());


        try
        {            
            Int32 col_seq_no = Convert.ToInt32(Session["show_avail_order_col_seq_grid2_current"]);
            
            if (col_seq_no > 0)
            {
                SqlCommand cmd_seq = new SqlCommand("update  column_seq_user set col_seq = col_seq-1  where col_seq > '" + col_seq_no + "' and user_name = '" + UserLogin.UserName + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' ", conn);

                cmd_seq.ExecuteNonQuery();
                SqlCommand cmd_update = new SqlCommand("update column_seq_user set display = 1, col_seq = '" + Convert.ToInt32(Session["show_avail_order_seq_grid2"]) + "'  where seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq_grid2"]) + "' and user_name = '" + UserLogin.UserName + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' ", conn);
                cmd_update.ExecuteNonQuery();
                conn.Close();
                BuidDataSource();
                grid2databind();
            }
        }
        catch (Exception ex)
        {
            lbl_error.Text = ex.Message;
            conn.Close();
        }
        finally
        {
            conn.Close();
        }
    }
    protected void GridView2_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["show_avail_order_seq_grid2_index"] = GridView2.SelectedIndex;
        Session["show_avail_order_seq_grid2"] = ((Label)GridView2.SelectedRow.FindControl("seq_label")).Text;
        Session["show_avail_order_col_seq_grid2_current"] = ((Label)GridView2.SelectedRow.FindControl("col_seq_label")).Text;

        //Session["show_avail_order_col_seq_grid2_previous"] = ((Label)GridView2.SelectedRow.FindControl("col_seq_label")).Text;
        //Response.Write(Session["show_avail_order_seq_grid2_index"]);
    }
    protected void up_button_click(object sender, ImageClickEventArgs e)
    {
        try
        {
            //int col_seq_no = 0;
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();
            UserClass UserLogin = (UserClass)Session["User"];
            
            try
            {
                //Int32 current_col_val = Convert.ToInt32(Session["show_avail_order_col_seq_grid2_current"]);
                Int32 current_col_val2 = Convert.ToInt32(Session["show_avail_order_seq_grid2_index"]);
                if ((current_col_val2 + 1) > 1)
                {
                    Int32 current_col_val = Convert.ToInt32(Session["show_avail_order_seq_grid2_index"]) + 1;
                    Int32 new_col_val = current_col_val - 1;
                    //Response.Write(new_col_val);
                    string cmd_prev_col_seq = "select seq_no from column_seq_user where user_name = '" + UserLogin.UserName + "' and col_seq = '" + new_col_val + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' and display = 0";
                    //Response.Write(cmd_prev_col_seq);
                    SqlDataAdapter da = new SqlDataAdapter(cmd_prev_col_seq, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    //Response.Write(ds.Tables[0].Rows[0][0].ToString());


                    SqlCommand cmd_update = new SqlCommand("update column_seq_user set col_seq = '" + new_col_val + "'  where user_name = '" + UserLogin.UserName + "' and seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq_grid2"]) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' and display = 0 ", conn);
                    SqlCommand cmd_update_2 = new SqlCommand("update column_seq_user set col_seq = '" + current_col_val + "'  where user_name = '" + UserLogin.UserName + "' and seq_no = '" + Convert.ToInt32(ds.Tables[0].Rows[0][0].ToString()) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' and display = 0 ", conn);
                    //Response.Write(cmd_update.ToString());    
                    cmd_update.ExecuteNonQuery();
                    cmd_update_2.ExecuteNonQuery();

                    conn.Close();
                    BuidDataSource();
                    grid2databind();

                    //Response.Write(Session["show_avail_order_col_seq_grid2_current"]);
                }
            }
            catch (Exception ex)
            {
                lbl_error.Text = ex.Message;
                conn.Close();
            }
            finally
            {
                if(Convert.ToInt32(Session["show_avail_order_seq_grid2_index"])>0)
                Session["show_avail_order_seq_grid2_index"] = Convert.ToInt32(Session["show_avail_order_seq_grid2_index"]) - 1;
                Response.Write("<script>window.location.href = 'show_avail_order_entry.aspx';</script>");
                conn.Close();
                //GridView2_PreRender(sender, e);
            }
        }
        catch (Exception ex)
        {
            lbl_error.Text = "Sorry! already uppermost element ";
        }
    }
    protected void down_button_click(object sender, ImageClickEventArgs e)
    {
        try
        {
            //int col_seq_no = 0;
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();
            UserClass UserLogin = (UserClass)Session["User"];
            //string cmd2 = "select col_seq from column_seq_user where  seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq_grid2"]) + "' and sub_program = '" + Convert.ToString(Session["sub_prgrm_url"]) + "'";
            //SqlDataAdapter dap2 = new SqlDataAdapter(cmd2, conn);
            //DataSet das2 = new DataSet();
            //dap2.Fill(das2);
            //col_seq_no = Convert.ToInt32(das2.Tables[0].Rows[0][0].ToString());
            //int select_col = col_seq_no + 1;

            //string cmd = "select seq_no from column_seq_user where  col_seq = '" + select_col + "' and sub_program = '" + Convert.ToString(Session["sub_prgrm_url"]) + "'";
            //SqlDataAdapter dap = new SqlDataAdapter(cmd, conn);
            //DataSet das = new DataSet();
            //dap.Fill(das);
            //int select_seq = Convert.ToInt32(das.Tables[0].Rows[0][0].ToString());
            //Response.Write(select_seq);
            //int down_seq_no = col_seq_no - 1;
            //Response.Write(select_seq);


            try
            {
                //SqlCommand cmd_update = new SqlCommand("update column_seq_user set col_seq = col_seq + 1  where seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq_grid2"]) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' ", conn);
                //cmd_update.ExecuteNonQuery();

                //SqlCommand cmd_seq = new SqlCommand("update column_seq_user set col_seq = col_seq - 1  where  seq_no = '" + select_seq + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' ", conn);
                //cmd_seq.ExecuteNonQuery();
                //conn.Close();
                //BuidDataSource();
                //grid2databind();

                
                //Int32 current_col_val = Convert.ToInt32(Session["show_avail_order_col_seq_grid2_current"]);
                Int32 current_col_val2 = Convert.ToInt32(Session["show_avail_order_seq_grid2_index"]);
                if (current_col_val2 < GridView2.Rows.Count + 1)
                {
                    Int32 current_col_val = Convert.ToInt32(Session["show_avail_order_seq_grid2_index"]) + 1;
                    Int32 new_col_val = current_col_val + 1;

                    //Response.Write(new_col_val);
                    string cmd_prev_col_seq = "select seq_no from column_seq_user where user_name = '" + UserLogin.UserName + "' and col_seq = '" + new_col_val + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' and display = 0";
                    //Response.Write(cmd_prev_col_seq);
                    SqlDataAdapter da = new SqlDataAdapter(cmd_prev_col_seq, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    //Response.Write(ds.Tables[0].Rows[0][0].ToString());

                    //Response.Write(new_col_val);
                    //Response.Write(current_col_val);
                    SqlCommand cmd_update = new SqlCommand("update column_seq_user set col_seq = '" + new_col_val + "'  where user_name = '" + UserLogin.UserName + "' and seq_no = '" + Convert.ToInt32(Session["show_avail_order_seq_grid2"]) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' and display = 0 ", conn);
                    SqlCommand cmd_update_2 = new SqlCommand("update column_seq_user set col_seq = '" + current_col_val + "'  where user_name = '" + UserLogin.UserName + "' and seq_no = '" + Convert.ToInt32(ds.Tables[0].Rows[0][0].ToString()) + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' and display = 0 ", conn);
                    //Response.Write(cmd_update.ToString());    
                    cmd_update.ExecuteNonQuery();
                    cmd_update_2.ExecuteNonQuery();


                    //SqlCommand cmd_seq = new SqlCommand("update column_seq_user set col_seq = col_seq + 1  where  seq_no = '" + select_seq + "' and sub_program ='" + Convert.ToString(Session["sub_prgrm_url"]) + "' ", conn);
                    //cmd_seq.ExecuteNonQuery();
                    conn.Close();
                    BuidDataSource();
                    grid2databind();
                }
            }
            catch (Exception ex)
            {
                lbl_error.Text = ex.Message;
                conn.Close();
            }
            finally
            {
                if(Convert.ToInt32(Session["show_avail_order_seq_grid2_index"])<GridView2.Rows.Count - 1 )
                Session["show_avail_order_seq_grid2_index"] = Convert.ToInt32(Session["show_avail_order_seq_grid2_index"]) + 1;
                Response.Write("<script>window.location.href = 'show_avail_order_entry.aspx';</script>");
                conn.Close();
            }
        }
        catch (Exception ex)
        {
            lbl_error.Text = "Sorry! already lowermost element";
        }
    }

    protected void GridView1_PreRender(object sender, EventArgs e)
    {
        BuidDataSource();
    }
    protected void GridView2_PreRender(object sender, EventArgs e)
    {
        grid2databind();
    }
}
