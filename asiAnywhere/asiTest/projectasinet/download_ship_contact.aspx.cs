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

public partial class download_ship : System.Web.UI.Page
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
            string vPage = "download_ship_contact.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            Session["s_user_comp"] = lblComp.Text;
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
                Session["s_user_login"] = lblUser.Text;
            }
        }

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }

        GridView1.SelectedIndex = Convert.ToInt32(Session["down_s_index"]);
        if (Session["down_s_index"] == null)
        {
            GridView1.SelectedIndex = 0;
            foreach (GridViewRow gv in GridView1.Rows)
            {
                //Session["s_comp"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
                //Session["s_cust_no"] = ((Label)GridView1.SelectedRow.FindControl("Label2")).Text;
                //Session["s_shipid"] = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
                //Session["s_first_name"] = ((Label)GridView1.SelectedRow.FindControl("Label5")).Text;
                //Session["s_last_name"] = ((Label)GridView1.SelectedRow.FindControl("Label6")).Text;
                //Session["s_cont_title"] = ((Label)GridView1.SelectedRow.FindControl("Label9")).Text;

                //Session["s_cont_loc"] = ((Label)GridView1.SelectedRow.FindControl("Label10")).Text;
                //Session["s_cust_name"] = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
                //Session["s_addr1"] = ((Label)GridView1.SelectedRow.FindControl("Label12")).Text;
                //Session["s_addr2"] = ((Label)GridView1.SelectedRow.FindControl("Label13")).Text;
                //Session["s_city"] = ((Label)GridView1.SelectedRow.FindControl("Label14")).Text;
                //Session["s_state"] = ((Label)GridView1.SelectedRow.FindControl("Label15")).Text;
                //Session["s_zip"] = ((Label)GridView1.SelectedRow.FindControl("Label16")).Text;
                //Session["s_country"] = ((Label)GridView1.SelectedRow.FindControl("Label17")).Text;
                //Session["s_terr"] = ((Label)GridView1.SelectedRow.FindControl("Label18")).Text;
                //Session["s_phone"] = ((Label)GridView1.SelectedRow.FindControl("Label20")).Text;
                //Session["s_fax"] = ((Label)GridView1.SelectedRow.FindControl("Label22")).Text;
                //Session["s_ext"] = ((Label)GridView1.SelectedRow.FindControl("Label23")).Text;
                //Session["s_email"] = ((Label)GridView1.SelectedRow.FindControl("Label24")).Text;
                //Session["s_rec"] = ((Label)GridView1.SelectedRow.FindControl("Label26")).Text;

            }
        }
        
            DataSet old = new DataSet();
            download dd = new download();
            old = dd.shiplist("select", Convert.ToString(Session["s_user_login"]), Convert.ToString(Session["s_user_comp"]),txt_custno.Text.Trim());

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
     

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["down_s_index"] = GridView1.SelectedIndex;
        foreach (GridViewRow gv in GridView1.Rows)
        {
            //Session["s_comp"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
            //Session["s_cust_no"] = ((Label)GridView1.SelectedRow.FindControl("Label2")).Text;
            //Session["s_shipid"] = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
            //Session["s_first_name"] = ((Label)GridView1.SelectedRow.FindControl("Label5")).Text;
            //Session["s_last_name"] = ((Label)GridView1.SelectedRow.FindControl("Label6")).Text;
            //Session["s_cont_title"] = ((Label)GridView1.SelectedRow.FindControl("Label9")).Text;
            
            //Session["s_cont_loc"] = ((Label)GridView1.SelectedRow.FindControl("Label10")).Text;
            //Session["s_cust_name"] = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
            //Session["s_addr1"] = ((Label)GridView1.SelectedRow.FindControl("Label12")).Text;
            //Session["s_addr2"] = ((Label)GridView1.SelectedRow.FindControl("Label13")).Text;
            //Session["s_city"] = ((Label)GridView1.SelectedRow.FindControl("Label14")).Text;
            //Session["s_state"] = ((Label)GridView1.SelectedRow.FindControl("Label15")).Text;
            //Session["s_zip"] = ((Label)GridView1.SelectedRow.FindControl("Label16")).Text;
            //Session["s_country"] = ((Label)GridView1.SelectedRow.FindControl("Label17")).Text;
            //Session["s_terr"] = ((Label)GridView1.SelectedRow.FindControl("Label18")).Text;
            //Session["s_phone"] = ((Label)GridView1.SelectedRow.FindControl("Label20")).Text;
            //Session["s_fax"] = ((Label)GridView1.SelectedRow.FindControl("Label22")).Text;
            //Session["s_ext"] = ((Label)GridView1.SelectedRow.FindControl("Label23")).Text;
            //Session["s_email"] = ((Label)GridView1.SelectedRow.FindControl("Label24")).Text;
            //Session["s_rec"] = ((Label)GridView1.SelectedRow.FindControl("Label26")).Text;
            
        }
                        
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        //int j = rec.Length-1;
        
        //for (int i = 0; i < rec.Length; i++)
        //{
        //    if (Convert.ToInt64(Session["s_rec"]) == rec[i])
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
        //            SqlCommand cmd = new SqlCommand("insert into contact (company,cust_no,ship_id,first_name,last_name,contact_title,contact_loc,cust_name,addr1,addr2,city, state,zip,country,territory,phone,fax,extension,email,rec_key,maillist) values ('" + Session["s_comp"] + "','" + Session["s_cust_no"] + "','" + Session["s_shipid"] + "','" + Session["s_first_name"] + "','" + Session["s_last_name"] + "','" + Session["s_cont_title"] + "','" + Session["s_cont_loc"] + "','" + Session["s_cust_name"] + "','" + Session["s_addr1"] + "','" + Session["s_addr2"] + "','" + Session["s_city"] + "','" + Session["s_state"] + "','" + Session["s_zip"] + "','" + Session["s_country"] + "','" + Session["s_terr"] + "','" + Session["s_phone"] + "','" + Session["s_fax"] + "','" + Session["s_ext"] + "','" + Session["s_email"] + "', '" + Session["s_rec"] + "','" + "False" + "')", conn);
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
                    //Response.Write(Session["d_user_comp"]);
                    //Response.Write(((Label)row.FindControl("Label2")).Text);
                    //Response.Write(((Label)row.FindControl("Label3")).Text);
                    //Response.Write(((Label)row.FindControl("Label5")).Text);
                    //Response.Write(((Label)row.FindControl("Label6")).Text);

                    //Response.Write(((Label)row.FindControl("Label9")).Text);
                    //Response.Write(((Label)row.FindControl("Label10")).Text);
                    //Response.Write(((Label)row.FindControl("Label11")).Text);

                    //Response.Write(((Label)row.FindControl("Label12")).Text);
                    //Response.Write(((Label)row.FindControl("Label13")).Text);
                    //Response.Write(((Label)row.FindControl("Label14")).Text);
                    //Response.Write(((Label)row.FindControl("Label15")).Text);

                    //Response.Write(((Label)row.FindControl("Label16")).Text);
                    //Response.Write(((Label)row.FindControl("Label17")).Text);
                    //Response.Write(((Label)row.FindControl("Label18")).Text);
                    //Response.Write(((Label)row.FindControl("Label20")).Text);

                    //Response.Write(((Label)row.FindControl("Label22")).Text);
                    //Response.Write(((Label)row.FindControl("Label23")).Text);
                    //Response.Write(((Label)row.FindControl("Label24")).Text);
                    //Response.Write(((Label)row.FindControl("Label26")).Text);



                    SqlCommand cmd = new SqlCommand("insert into contact (company,cust_no,ship_id,first_name,last_name,contact_title,contact_loc,cust_name,addr1,addr2,city, state,zip,country,territory,phone,fax,extension,email,rec_key,maillist) values ('" + ((Label)row.FindControl("Label1")).Text + "','" + ((Label)row.FindControl("Label2")).Text + "','" + ((Label)row.FindControl("Label3")).Text + "','" + ((Label)row.FindControl("Label5")).Text + "','" + ((Label)row.FindControl("Label6")).Text + "','" + ((Label)row.FindControl("Label9")).Text + "','" + "C" + "','" + ((Label)row.FindControl("Label11")).Text + "','" + ((Label)row.FindControl("Label12")).Text + "','" + ((Label)row.FindControl("Label13")).Text + "','" + ((Label)row.FindControl("Label14")).Text + "','" + ((Label)row.FindControl("Label15")).Text + "','" + ((Label)row.FindControl("Label16")).Text + "','" + ((Label)row.FindControl("Label17")).Text + "','" + ((Label)row.FindControl("Label18")).Text + "','" + ((Label)row.FindControl("Label20")).Text + "','" + ((Label)row.FindControl("Label22")).Text + "','" + ((Label)row.FindControl("Label23")).Text + "', '" + ((Label)row.FindControl("Label24")).Text + "','" + ((Label)row.FindControl("Label26")).Text + "','"+ "False" +"')", conn);

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