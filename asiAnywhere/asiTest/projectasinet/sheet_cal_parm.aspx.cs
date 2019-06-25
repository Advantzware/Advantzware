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

public partial class sheet_cal_parm : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        try
        {
            UserClass UserLogin = (UserClass)Session["User"];
           
        }
        catch
        {
            Response.Write("Session Expired! Please Relogin");
        }
        if (!Page.IsPostBack)
        {
             SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
             try
             {
                 conn.Open();

                 string cmd = "select * from report_maintance where user_name = '" + Convert.ToString(Session["User"]) + "' and prog_name = 'sheet_cal_parm.aspx'  ";
                 SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                 DataSet ds = new DataSet();
                 da.Fill(ds);
                 foreach (DataRow dr in ds.Tables[0].Rows)
                 {
                     machineTextBox.Text = dr["field1"].ToString();
                     itemTextBox.Text = dr["field2"].ToString();

                 }
             }
             catch { }
            finally 
                    {
                        conn.Close();
                    }

            checkBox1.Checked = true;
            RadioButtonList1.SelectedIndex = 1;
        }
    }
    protected void save_button_Click(object sender, EventArgs e)
    {
        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "N";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "C";
        if (checkBox1.Checked == true)
            HiddenField2.Value = "Yes";
        else
            HiddenField2.Value = "No";

        Corrugated corr = new Corrugated();
        corr.foldsheetcal("sheet-cal", Convert.ToString(Session["User"]), Convert.ToString(Session["order_folding_est"]), machineTextBox.Text, Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), itemTextBox.Text.Trim(), HiddenField1.Value, HiddenField2.Value);


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + Convert.ToString(Session["User"]) + "' and prog_name = 'sheet_cal_parm.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2 ) values ('" + Convert.ToString(Session["User"]) + "','sheet_cal_parm.aspx','" + machineTextBox.Text.Trim() + "','" + itemTextBox.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + machineTextBox.Text.Trim() + "', field2 = '" + itemTextBox.Text.Trim() + "' where user_name = '" + Convert.ToString(Session["User"]) + "' and prog_name ='sheet_cal_parm.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

            conn.Close();
        }
        catch
        {
            conn.Close();
        }
        finally
        {
            conn.Close();
        }

        Response.Write("<script>window.opener.location='fold_layout.aspx';self.close();</script>");
       


    }
    
}
