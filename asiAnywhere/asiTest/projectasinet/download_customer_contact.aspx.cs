using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET1;
using ASIDataNS;
using Progress.Open4GL.Proxy;
using System.Data.SqlClient;

/// <summary>
/// Summary description for Class1
/// </summary>

public partial class download_customers : System.Web.UI.Page
{
        Int64[] rec;
        int i = 0;
        
    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "download_customer_contact.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            Session["d_user_comp"] = lblComp.Text;
            if (aUsers == "external")
            {
               
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
                Session["d_user_login"] = lblUser.Text;
            }
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }

        GridView1.SelectedIndex = Convert.ToInt32(Session["down_index"]);
        if (Session["down_index"] == null)
        {
            GridView1.SelectedIndex = 0;
            foreach (GridViewRow gv in GridView1.Rows)
            {
                //Session["d_comp"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
                //Session["d_cust_no"] = ((Label)GridView1.SelectedRow.FindControl("Label2")).Text;
                //Session["d_sman"] = ((Label)GridView1.SelectedRow.FindControl("Label4")).Text;
                //Session["d_first_name"] = ((Label)GridView1.SelectedRow.FindControl("Label5")).Text;
                //Session["d_last_name"] = ((Label)GridView1.SelectedRow.FindControl("Label25")).Text;
                //Session["d_cont_title"] = ((Label)GridView1.SelectedRow.FindControl("Label6")).Text;
                //Session["d_type"] = ((Label)GridView1.SelectedRow.FindControl("Label7")).Text;
                //Session["d_cont_loc"] = "C";
                //Session["d_cust_name"] = ((Label)GridView1.SelectedRow.FindControl("Label9")).Text;
                //Session["d_addr1"] = ((Label)GridView1.SelectedRow.FindControl("Label10")).Text;
                //Session["d_addr2"] = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
                //Session["d_city"] = ((Label)GridView1.SelectedRow.FindControl("Label12")).Text;
                //Session["d_state"] = ((Label)GridView1.SelectedRow.FindControl("Label13")).Text;
                //Session["d_zip"] = ((Label)GridView1.SelectedRow.FindControl("Label14")).Text;
                //Session["d_terr"] = ((Label)GridView1.SelectedRow.FindControl("Label15")).Text;
                //Session["d_phone"] = ((Label)GridView1.SelectedRow.FindControl("Label16")).Text;
                //Session["d_fax"] = ((Label)GridView1.SelectedRow.FindControl("Label17")).Text;
                //Session["d_ext"] = ((Label)GridView1.SelectedRow.FindControl("Label18")).Text;
                //Session["d_email"] = ((Label)GridView1.SelectedRow.FindControl("Label19")).Text;
                //Session["d_rec"] = ((Label)GridView1.SelectedRow.FindControl("Label20")).Text;

            }
        }
        
            DataSet old = new DataSet();
            download dd = new download();
            old = dd.contactlist("select",Convert.ToString(Session["d_user_login"]),Convert.ToString(Session["d_user_comp"]),txt_custno.Text.Trim());

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

            try
            {
                string cmd = "select * from contact";

                SqlDataAdapter danew = new SqlDataAdapter(cmd, conn);
                DataSet dsOld = new DataSet();
                DataSet dsNew = new DataSet();
                dsOld = old.Copy();
                danew.Fill(dsNew);

                DataTable dtOld = dsOld.Tables[0];
                DataTable dtNew = dsNew.Tables[0];
                Int64 n=dtNew.Rows.Count;
                rec = new Int64[n];
                for (int i = 0; i < dtNew.Rows.Count; i++)
                {

                    rec[i] = Convert.ToInt64(dtNew.Rows[i]["rec_key"]);
                    
                }
            }       

            catch
            {
                return;
            }
            finally
            {
                conn.Close();
            }
       

    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["down_index"] = GridView1.SelectedIndex;
        foreach (GridViewRow gv in GridView1.Rows)
        {
            //Session["d_comp"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
            //Session["d_cust_no"] = ((Label)GridView1.SelectedRow.FindControl("Label2")).Text;
            //Session["d_sman"] = ((Label)GridView1.SelectedRow.FindControl("Label4")).Text;
            //Session["d_first_name"] = ((Label)GridView1.SelectedRow.FindControl("Label5")).Text;
            //Session["d_last_name"] = ((Label)GridView1.SelectedRow.FindControl("Label25")).Text;
            //Session["d_cont_title"] = ((Label)GridView1.SelectedRow.FindControl("Label6")).Text;
            //Session["d_type"] = ((Label)GridView1.SelectedRow.FindControl("Label7")).Text;
            //Session["d_cont_loc"] = "C";
            //Session["d_cust_name"] = ((Label)GridView1.SelectedRow.FindControl("Label9")).Text;
            //Session["d_addr1"] = ((Label)GridView1.SelectedRow.FindControl("Label10")).Text;
            //Session["d_addr2"] = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
            //Session["d_city"] = ((Label)GridView1.SelectedRow.FindControl("Label12")).Text;
            //Session["d_state"] = ((Label)GridView1.SelectedRow.FindControl("Label13")).Text;
            //Session["d_zip"] = ((Label)GridView1.SelectedRow.FindControl("Label14")).Text;
            //Session["d_terr"] = ((Label)GridView1.SelectedRow.FindControl("Label15")).Text;
            //Session["d_phone"] = ((Label)GridView1.SelectedRow.FindControl("Label16")).Text;
            //Session["d_fax"] = ((Label)GridView1.SelectedRow.FindControl("Label17")).Text;
            //Session["d_ext"] = ((Label)GridView1.SelectedRow.FindControl("Label18")).Text;
            //Session["d_email"] = ((Label)GridView1.SelectedRow.FindControl("Label19")).Text;
            //Session["d_rec"] = ((Label)GridView1.SelectedRow.FindControl("Label20")).Text;
            
        }
        //Response.Write(Session["d_comp"]);
        //Response.Write(Session["d_cust_no"]);
        //Response.Write(Session["d_ship_id"]);
        //Response.Write(Session["d_sman"]);
        //Response.Write(Session["d_first_name"]);
        //Response.Write(Session["d_cont_title"]);
        //Response.Write(Session["d_type"]);

        //Response.Write(Session["d_cont_loc"]);
        //Response.Write(Session["d_cust_name"]);
        //Response.Write(Session["d_addr1"]);
        //Response.Write(Session["d_addr2"]);
        //Response.Write(Session["d_city"]);
        //Response.Write(Session["d_state"]);
        //Response.Write(Session["d_zip"]);
        //Response.Write(Session["d_terr"]);
        //Response.Write(Session["d_phone"]);
        //Response.Write(Session["d_fax"]);
        //Response.Write(Session["d_ext"]);
        //Response.Write(Session["d_email"]);
        //Response.Write(Session["d_rec"]);
        
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        //int j = rec.Length-1;
        //for (int i = 0; i < rec.Length; i++)
        //{
        //    if (Convert.ToInt64(Session["d_rec"]) == rec[i])
        //    {
        //        Response.Write("<script>alert('This Record Allready Exists')</script>");
        //        break;
        //    }
           
        //    if (j == i)
        //    {
        //        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        //        try
        //        {
        //            conn.Open();
        //            SqlCommand cmd = new SqlCommand("insert into contact (company,cust_no,sman,first_name,last_name,contact_title,type,contact_loc,cust_name,addr1,addr2,city, state,zip,territory,phone,fax,extension,email,rec_key,maillist) values ('" + Session["d_comp"] + "','" + Session["d_cust_no"] + "','" + Session["d_sman"] + "','" + Session["d_first_name"] + "','" + Session["d_last_name"] + "','" + Session["d_cont_title"] + "','" + Session["d_type"] + "','" + Session["d_cont_loc"] + "','" + Session["d_cust_name"] + "','" + Session["d_addr1"] + "','" + Session["d_addr2"] + "','" + Session["d_city"] + "','" + Session["d_state"] + "','" + Session["d_zip"] + "','" + Session["d_terr"] + "','" + Session["d_phone"] + "','" + Session["d_fax"] + "','" + Session["d_ext"] + "','" + Session["d_email"] + "', '" + Session["d_rec"] + "','" + "False" + "')", conn);
        //            cmd.ExecuteNonQuery();
        //            conn.Close();
        //        }
        //        catch
        //        {
        //            return;
        //        }
        //        finally
        //        {
        //            conn.Close();                    
        //        }
        //    }  
        //}  
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                try
                {
                    conn.Open();
                    //Response.Write(row.Cells[1].Text);
                    //Response.Write(row.Cells[2].Text);
                    //Response.Write(((Label)row.FindControl("Label1")).Text);
                    //Response.Write(row.Cells[4].Text);
                    //Response.Write(row.Cells[5].Text);
                    //Response.Write(row.Cells[6].Text);
                    //Response.Write(row.Cells[7].Text);
                    //Response.Write(row.Cells[8].Text);
                    //Response.Write(row.Cells[9].Text);
                    //Response.Write(row.Cells[10].Text);

                    //Response.Write(((Label)row.FindControl("Label2")).Text);
                    //Response.Write(((CheckBox)row.FindControl("CheckBox1")).Checked);
                    //Response.Write(((Label)row.FindControl("Label3")).Text);
                    //Response.Write(((Label)row.FindControl("Label4")).Text);
                    //Response.Write(((Label)row.FindControl("Label5")).Text);
                    //Response.Write(((Label)row.FindControl("Label6")).Text);
                    //Response.Write(((Label)row.FindControl("Label7")).Text);
                    //Response.Write(((Label)row.FindControl("Label8")).Text);
                    //Response.Write(((Label)row.FindControl("Label9")).Text);

                    //Response.Write(((Label)row.FindControl("Label10")).Text);
                     
                    SqlCommand cmd = new SqlCommand("insert into contact (company,cust_no,sman,first_name,last_name,contact_title,type,contact_loc,cust_name,addr1,addr2,city, state,zip,territory,phone,fax,extension,email,rec_key,maillist) values ('" + row.Cells[21].Text + "','" + row.Cells[1].Text + "','" + ((Label)row.FindControl("Label1")).Text + "','" + row.Cells[4].Text + "','" + row.Cells[5].Text + "','" + ((Label)row.FindControl("Label2")).Text + "','" + ((Label)row.FindControl("Label3")).Text + "','" + "C" + "','" + row.Cells[2].Text + "','" + row.Cells[6].Text + "','" + ((Label)row.FindControl("Label5")).Text + "','" + row.Cells[7].Text + "','" + row.Cells[8].Text + "','" + row.Cells[9].Text + "','" + ((Label)row.FindControl("Label6")).Text + "','" + row.Cells[10].Text + "','" + ((Label)row.FindControl("Label7")).Text + "','" + ((Label)row.FindControl("Label8")).Text + "','" + ((Label)row.FindControl("Label9")).Text + "', '" + ((Label)row.FindControl("Label10")).Text + "','" + "False" + "')", conn);
                                                                                                                                                                                                                                                        
                    cmd.ExecuteNonQuery();
                    conn.Close();
                }
                catch
                {
                    Response.Write("");
                }
                finally
                {
                    conn.Close();
                }
            }
        }

    }
    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        
        Response.Redirect(sMenuURL);
    }
     
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_custno.Text.Trim();
        Page_Load(sender, e);
    }
    protected void btnAll_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
        txt_custno.Text = "";
    }
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }

}